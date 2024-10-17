package ro.iugori.yadvs.aop.rest;

import jakarta.validation.Validator;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.delegate.rest.ErrorResponseBuilder;
import ro.iugori.yadvs.model.error.YadvsRestException;

@Aspect
@Component
public class RequestMappingAspect {

    private final Validator validator;

    public RequestMappingAspect(Validator validator) {
        this.validator = validator;
    }

    @Around("@annotation(org.springframework.web.bind.annotation.GetMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.PatchMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.PutMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.PostMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.DeleteMapping) || " +
            "@annotation(org.springframework.web.bind.annotation.RequestMapping)")
    public Object injectCallContext(ProceedingJoinPoint joinPoint) throws Throwable {
        var args = joinPoint.getArgs();
        var paramAnnotations = ((MethodSignature) joinPoint.getSignature()).getMethod().getParameterAnnotations();
        for (var i = 0; i < args.length; ++i) {
            for (var annotation : paramAnnotations[i]) {
                if (annotation instanceof Check) {
                    var validationResult = validator.validate(args[i]);
                    if (!validationResult.isEmpty()) {
                        var errors = ErrorResponseBuilder.responseOf(getRestContext(args), validationResult);
                        return new ResponseEntity<>(errors, HttpStatus.BAD_REQUEST);
                    }
                    break;
                }
            }
        }
        try {
            return joinPoint.proceed(args);
        } catch (Exception e) {
            if (e instanceof YadvsRestException) {
                throw e;
            }
            throw new YadvsRestException(getRestContext(args), e);
        }
    }

    private static RestContext getRestContext(Object[] args) {
        RestContext restCtx = null;
        for (var arg : args) {
            if (arg instanceof RestContext) {
                restCtx = (RestContext) arg;
                break;
            }
        }
        if (restCtx == null) {
            restCtx = new RestContext();
        }
        return restCtx;
    }

}
