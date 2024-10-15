package ro.iugori.yadvs.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.delegate.rest.ErrorResponseBuilder;
import ro.iugori.yadvs.model.domain.TargetType;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.YadvsException;

@RestControllerAdvice
@Slf4j
public class RestExceptionHandler {

    @ExceptionHandler(Exception.class)
    public ResponseEntity<Object> handleAnyException(Exception e) {
        var callCtx = (RestContext) ((e instanceof YadvsException ye) ? ye.getCallCtx() : new RestContext());
        callCtx.getLogger().error(callCtx.getTraceId(), e);

        if (e instanceof YadvsException ye) {
            var errorResponse = ErrorResponseBuilder.responseOf(ye);
            if (ye instanceof CheckException) {
                for (var error : ((CheckException) ye).getErrors()) {
                    if (ErrorCode.NOT_ALLOWED.code == error.getCodeAsInt()) {
                        return new ResponseEntity<>(errorResponse, HttpStatus.FORBIDDEN);
                    }
                }
                return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
            }
            return new ResponseEntity<>(errorResponse, HttpStatus.INTERNAL_SERVER_ERROR);
        }

        if (e instanceof MethodArgumentTypeMismatchException ex) {
            var errorResponse = ErrorResponseBuilder.responseOf(callCtx, e, TargetType.PARAMETER, ex.getName());
            return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
        }

        var errorResponse = ErrorResponseBuilder.responseOf(callCtx, e, TargetType.URI, callCtx.getRequest().getRequestURI());

        if (e instanceof HttpRequestMethodNotSupportedException) {
            return new ResponseEntity<>(errorResponse, HttpStatus.METHOD_NOT_ALLOWED);
        }

        return new ResponseEntity<>(errorResponse, HttpStatus.INTERNAL_SERVER_ERROR);
    }

}
