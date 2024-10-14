package ro.iugori.yadvs.aop.callcontext;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import ro.iugori.yadvs.delegate.ctx.RestContext;

@Aspect
@Component
public class CallContextAspect {

    @Around("@annotation(InjectCallContext)")
    public Object injectRestContext(ProceedingJoinPoint joinPoint) throws Throwable {
        var args = joinPoint.getArgs();
        var parameterTypes = ((MethodSignature) joinPoint.getSignature()).getParameterTypes();
        for (var i = 0; i < args.length; ++i) {
            var parameterClass = parameterTypes[i];
            if (RestContext.class.equals(parameterClass)) {
                var attributes = (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
                var callContext = new RestContext(attributes.getRequest());
                args[i] = callContext;
                break;
            }
        }
        return joinPoint.proceed(args);
    }

}
