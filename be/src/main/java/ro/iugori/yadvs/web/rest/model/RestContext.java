package ro.iugori.yadvs.web.rest.model;


import jakarta.servlet.http.HttpServletRequest;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.util.rest.RestApi;

import java.util.Locale;

@Slf4j
public class RestContext extends CallContext {

    public static RestContext fromRequestContextHolder() {
        var attributes = (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
        var httpServletRequest = attributes.getRequest();
        var acceptLinksHeader = httpServletRequest.getHeader(RestApi.Header.ACCEPT_LINKS);
        var fillHATEOASLinks = (acceptLinksHeader != null) && acceptLinksHeader.toUpperCase(Locale.ROOT).contains(RestApi.Header.Value.ACCEPT_LINKS_HATEOAS);
        return new RestContext(httpServletRequest, fillHATEOASLinks);
    }

    @Getter
    private final HttpServletRequest request;
    @Getter
    private final boolean fillHATEOASLinks;

    private RestContext(HttpServletRequest request, boolean fillHATEOASLinks) {
        super(RestContext.log);
        this.request = request;
        this.fillHATEOASLinks = fillHATEOASLinks;
    }

}

