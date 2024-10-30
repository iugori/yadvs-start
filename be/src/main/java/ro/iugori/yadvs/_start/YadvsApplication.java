package ro.iugori.yadvs._start;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import java.net.InetAddress;
import java.util.Optional;


@SpringBootApplication(scanBasePackageClasses = {
        ro.iugori.yadvs.web.rest.aop.RequestMappingAspect.class,
        ro.iugori.yadvs.web.rest.config.RestExceptionHandler.class,
        ro.iugori.yadvs.web.rest.api.PollResource.class,
        ro.iugori.yadvs.service.api.PollService.class,
        ro.iugori.yadvs.repository.config.CustomRepositoryConfig.class,
})
@EnableJpaRepositories(basePackageClasses = ro.iugori.yadvs.repository.api.PollRepository.class)
@EntityScan(basePackageClasses = ro.iugori.yadvs.model.entity.PollEntity.class)
@Slf4j
public class YadvsApplication {

    @SneakyThrows
    public static void main(String[] args) {
        var appCtx = SpringApplication.run(YadvsApplication.class, args);
        if (appCtx instanceof WebServerApplicationContext webCtx) {
            var webServer = webCtx.getWebServer();
            var url = new StringBuilder(String.format("http://%s:%d", InetAddress.getLocalHost().getHostAddress(), webServer.getPort()));
            Optional.ofNullable(webCtx.getServerNamespace())
                    .filter(StringUtils::isNotEmpty)
                    .map(path -> url.append("/").append(path));
            url.append("/swagger-ui.html");
            log.info("Swagger URL is {}", url);
        }
    }

}
