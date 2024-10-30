package ro.iugori.yadvs.repository.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import ro.iugori.yadvs.repository.api.PollRepositoryCustom;
import ro.iugori.yadvs.repository.impl.PollRepositoryImpl;

@Configuration
public class CustomRepositoryConfig {
    @Bean
    public PollRepositoryCustom pollRepositoryCustom() {
        return new PollRepositoryImpl();
    }

}
