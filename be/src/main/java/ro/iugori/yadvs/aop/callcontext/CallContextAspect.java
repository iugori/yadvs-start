package ro.iugori.yadvs.aop.callcontext;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;
import ro.iugori.yadvs.delegate.ctx.RestContext;

@Aspect
@Component
public class CallContextAspect {

    @Around("@annotation(InjectCallContext) || @within(InjectCallContext)")
    public Object injectCallContext(ProceedingJoinPoint joinPoint) throws Throwable {
        var args = joinPoint.getArgs();
        var paramTypes = ((MethodSignature) joinPoint.getSignature()).getParameterTypes();
        for (var i = 0; i < args.length; ++i) {
            var parameterClass = paramTypes[i];
            if (RestContext.class.equals(parameterClass) && args[i] == null) {
                args[i] = new RestContext();
                break;
            }
        }
        return joinPoint.proceed(args);
    }

}
