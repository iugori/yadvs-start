package ro.iugori.yadvs.config;

import org.springframework.core.MethodParameter;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;
import ro.iugori.yadvs.model.rest.RestContext;

@Component
public class RestContextArgumentResolver implements HandlerMethodArgumentResolver {

    @Override
    public boolean supportsParameter(MethodParameter parameter) {
        return RestContext.class.isAssignableFrom(parameter.getParameterType());
    }

    @Override
    public Object resolveArgument(@NonNull MethodParameter parameter
            , ModelAndViewContainer mavContainer
            , @NonNull NativeWebRequest webRequest
            , WebDataBinderFactory binderFactory) {
        return RestContext.fromRequestContextHolder();
    }

}
