package ro.iugori.yadvs.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.List;

@Configuration
public class WebConfig implements WebMvcConfigurer {

    private final RestContextArgumentResolver restContextArgumentResolver;

    public WebConfig(RestContextArgumentResolver restContextArgumentResolver) {
        this.restContextArgumentResolver = restContextArgumentResolver;
    }

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.add(restContextArgumentResolver);
    }

}