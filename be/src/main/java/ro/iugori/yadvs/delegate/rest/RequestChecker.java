package ro.iugori.yadvs.delegate.rest;

import jakarta.servlet.http.HttpServletRequest;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import ro.iugori.yadvs.util.rest.RestApi;

public class RequestChecker {

    public static void checkPatchContentType(HttpServletRequest request) throws HttpMediaTypeNotSupportedException {
        var contentType = request.getHeader(HttpHeaders.CONTENT_TYPE);
        if (StringUtils.isNotEmpty(contentType)
                && contentType.toLowerCase().contains(RestApi.MediaType.APPLICATION_JSON_PATCH_JSON.toString())) {
            throw new HttpMediaTypeNotSupportedException(
                    "Unsupported patch type `" + RestApi.MediaType.APPLICATION_JSON_PATCH_JSON + "'");
        }
    }

}
