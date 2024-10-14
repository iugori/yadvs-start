package ro.iugori.yadvs.delegate.ctx;

import jakarta.servlet.http.HttpServletRequest;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class RestContext extends CallContext {

    @Getter
    private final HttpServletRequest request;

    public RestContext(HttpServletRequest request) {
        super(RestContext.log);
        this.request = request;
    }

}
