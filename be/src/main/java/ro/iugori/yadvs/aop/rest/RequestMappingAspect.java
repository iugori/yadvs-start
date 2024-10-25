package ro.iugori.yadvs.aop.rest;

import jakarta.validation.Validator;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import ro.iugori.yadvs.delegate.rest.ErrorResponseBuilder;
import ro.iugori.yadvs.delegate.rest.ResponseEntityBuilder;
import ro.iugori.yadvs.model.error.YadvsRestException;
import ro.iugori.yadvs.model.rest.ctx.RestContext;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.IntStream;

@Aspect
@Component
public class RequestMappingAspect {

    private final Validator validator;

    public RequestMappingAspect(Validator validator) {
        this.validator = validator;
    }

    private static RestContext getRestContext(Object[] args) {
        return Arrays.stream(args)
                .filter(RestContext.class::isInstance)
                .map(RestContext.class::cast)
                .findFirst()
                .orElseGet(RestContext::fromRequestContextHolder);
    }

    @Around("@annotation(org.springframework.web.bind.annotation.GetMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.PatchMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.PutMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.PostMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.DeleteMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.RequestMapping)")
    public Object injectCallContext(ProceedingJoinPoint joinPoint) throws Throwable {
        var errorResponse = checkArgs(joinPoint);
        if (errorResponse.isPresent()) {
            return errorResponse.get();
        }
        var args = joinPoint.getArgs();
        try {
            return patchResponse(joinPoint.proceed(args), args);
        } catch (Exception e) {
            throw maybeWrapAsYadvsException(e, args);
        }
    }

    private Optional<ResponseEntity<?>> checkArgs(ProceedingJoinPoint joinPoint) {
        var args = joinPoint.getArgs();
        var paramAnnotations = ((MethodSignature) joinPoint.getSignature()).getMethod().getParameterAnnotations();

        return IntStream.range(0, args.length)
                .filter(i -> Arrays.stream(paramAnnotations[i]).anyMatch(Check.class::isInstance))
                .mapToObj(i -> validator.validate(args[i]))
                .filter(validationResult -> !validationResult.isEmpty())
                .findFirst()
                .map(validationResult -> {
                    var errors = ErrorResponseBuilder.of(getRestContext(args), validationResult);
                    return ResponseEntityBuilder.of(errors, HttpStatus.BAD_REQUEST);
                });
    }

    private static Object patchResponse(Object response, Object[] args) {
        if (response instanceof ResponseEntity<?> entity) {
            return ResponseEntityBuilder.withXCorrelationID(entity, getRestContext(args).getLogRef());
        }
        return response;
    }

    private static YadvsRestException maybeWrapAsYadvsException(Exception ex, Object[] args) {
        if (ex instanceof YadvsRestException yadvsException) {
            return yadvsException;
        }
        return new YadvsRestException(getRestContext(args), ex);
    }

}
