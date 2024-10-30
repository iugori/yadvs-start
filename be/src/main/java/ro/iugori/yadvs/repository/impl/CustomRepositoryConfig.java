package ro.iugori.yadvs.repository.impl;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import ro.iugori.yadvs.repository.core.PollRepositoryCustom;

@Configuration
public class CustomRepositoryConfig {
    @Bean
    public PollRepositoryCustom pollRepositoryCustom() {
        return new PollRepositoryImpl();
    }

}
