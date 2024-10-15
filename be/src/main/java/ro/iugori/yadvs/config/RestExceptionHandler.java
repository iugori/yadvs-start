package ro.iugori.yadvs.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.delegate.rest.ErrorBuilder;
import ro.iugori.yadvs.model.rest.TargetType;

@RestControllerAdvice
@Slf4j
public class RestExceptionHandler {

    @ExceptionHandler(Exception.class)
    public ResponseEntity<Object> handleAnyException(Exception e) {
        var callCtx = new RestContext();
        callCtx.getLogger().error(callCtx.getTraceId(), e);

        if (e instanceof MethodArgumentTypeMismatchException ex) {
            var errorResponse = ErrorBuilder.responseOf(callCtx, e, TargetType.PARAMETER, ex.getName());
            return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
        }

        var errorResponse = ErrorBuilder.responseOf(callCtx, e, TargetType.URI, callCtx.getRequest().getRequestURI());

        if (e instanceof HttpRequestMethodNotSupportedException) {
            return new ResponseEntity<>(errorResponse, HttpStatus.METHOD_NOT_ALLOWED);
        }

        return new ResponseEntity<>(errorResponse, HttpStatus.INTERNAL_SERVER_ERROR);
    }

}
