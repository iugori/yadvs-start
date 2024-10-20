package ro.iugori.yadvs.aop.rest;

import jakarta.validation.Validator;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import ro.iugori.yadvs.delegate.rest.ErrorResponseBuilder;
import ro.iugori.yadvs.model.ctx.RestContext;
import ro.iugori.yadvs.model.error.YadvsRestException;
import ro.iugori.yadvs.web.RestApi;

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
                        var restCtx = getRestContext(args);
                        var headers = new HttpHeaders();
                        headers.add(RestApi.Header.X_CORRELATION_ID, restCtx.getLogRef());
                        var errors = ErrorResponseBuilder.errorsOf(restCtx, validationResult);
                        return new ResponseEntity<>(errors, headers, HttpStatus.BAD_REQUEST);
                    }
                    break;
                }
            }
        }
        try {
            var r = joinPoint.proceed(args);
            if (r instanceof ResponseEntity<?> rr) {
                if (!rr.getHeaders().containsKey(RestApi.Header.X_CORRELATION_ID)) {
                    var headers = new HttpHeaders();
                    headers.addAll(rr.getHeaders());
                    headers.add(RestApi.Header.X_CORRELATION_ID, getRestContext(args).getLogRef());
                    r = new ResponseEntity<>(rr.getBody(), headers, rr.getStatusCode());
                }
            }
            return r;
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
