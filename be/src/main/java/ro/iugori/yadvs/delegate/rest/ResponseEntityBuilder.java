package ro.iugori.yadvs.delegate.rest;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import ro.iugori.yadvs.model.rest.ErrorResponse;
import ro.iugori.yadvs.util.rest.RestApi;

public class ResponseEntityBuilder {

    public static ResponseEntity<?> of(ErrorResponse body, HttpStatus status) {
        body.setStatus(status.value());
        var headers = new HttpHeaders();
        headers.add(RestApi.Header.X_CORRELATION_ID, body.getLogRef());
        return new ResponseEntity<>(body, headers, status);
    }

    public static ResponseEntity<?> withXCorrelationID(ResponseEntity<?> entity, String logRef) {
        var oldHeaders = entity.getHeaders();
        if (!oldHeaders.containsKey(RestApi.Header.X_CORRELATION_ID)) {
            var newHeaders = new HttpHeaders();
            newHeaders.addAll(oldHeaders);
            newHeaders.add(RestApi.Header.X_CORRELATION_ID, logRef);
            entity = new ResponseEntity<>(entity.getBody(), newHeaders, entity.getStatusCode());
        }
        return entity;
    }

}
