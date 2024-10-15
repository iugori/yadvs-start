package ro.iugori.yadvs;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.context.WebServerApplicationContext;

import java.net.InetAddress;
import java.util.Optional;

@SpringBootApplication
@Slf4j
public class YadvsApplication {

    @SneakyThrows
    public static void main(String[] args) {
        var appCtx = SpringApplication.run(YadvsApplication.class, args);
        if (appCtx instanceof WebServerApplicationContext webCtx) {
            var webServer = webCtx.getWebServer();
            var url = new StringBuilder(String.format("http://%s:%d", InetAddress.getLocalHost().getHostName(), webServer.getPort()));
            Optional.ofNullable(webCtx.getServerNamespace())
                    .filter(StringUtils::isNotEmpty)
                    .map(path -> url.append("/").append(path));
            url.append("/swagger-ui.html");
            log.info("Swagger URL is {}", url);
        }
    }

}
