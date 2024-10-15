package ro.iugori.yadvs.aop.validation;

import jakarta.validation.Validator;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.delegate.rest.ErrorBuilder;

@Aspect
@Component
public class CheckAspect {

    private final Validator validator;

    public CheckAspect(Validator validator) {
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
                        var errors = ErrorBuilder.responseOf(getRestContext(args), validationResult);
                        return new ResponseEntity<>(errors, HttpStatus.BAD_REQUEST);
                    }
                    break;
                }
            }
        }
        return joinPoint.proceed(args);
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
