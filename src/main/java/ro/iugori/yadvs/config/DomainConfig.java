package ro.iugori.yadvs.config;

import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;


@Configuration
@EntityScan("ro.iugori.yadvs.domain")
@EnableJpaRepositories("ro.iugori.yadvs.repos")
@EnableTransactionManagement
public class DomainConfig {
}
