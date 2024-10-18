package ro.iugori.yadvs.model.ctx;

import jakarta.servlet.http.HttpServletRequest;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

@Slf4j
public class RestContext extends CallContext {

    @Getter
    private final HttpServletRequest request;

    public RestContext() {
        super(RestContext.log);
        var attributes = (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
        this.request = attributes.getRequest();
    }

}
