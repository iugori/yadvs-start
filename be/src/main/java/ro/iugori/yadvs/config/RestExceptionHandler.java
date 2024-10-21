package ro.iugori.yadvs.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import ro.iugori.yadvs.delegate.rest.ErrorResponseBuilder;
import ro.iugori.yadvs.delegate.rest.ResponseEntityBuilder;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.ctx.RestContext;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.error.YadvsRestException;

import java.util.Objects;

@RestControllerAdvice
@Slf4j
public class RestExceptionHandler {

    @ExceptionHandler(YadvsRestException.class)
    public ResponseEntity<?> handleYadvsRestException(YadvsRestException ex) {
        var exCause = ex.getCause();
        var callCtx = ex.getCallCtx();

        logException(callCtx, Objects.requireNonNullElse(exCause, ex));

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (exCause instanceof CheckException) {
            httpStatus = HttpStatus.BAD_REQUEST;
        } else if (exCause instanceof HttpMediaTypeNotSupportedException) {
            httpStatus = HttpStatus.UNSUPPORTED_MEDIA_TYPE;
        }

        var errorResponse = ErrorResponseBuilder.of(ex);
        return ResponseEntityBuilder.of(errorResponse, httpStatus);
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<?> handleMethodArgumentTypeMismatchException(MethodArgumentTypeMismatchException e) {
        var callCtx = new RestContext();
        logException(callCtx, e);
        var errorResponse = ErrorResponseBuilder.of(callCtx, e, TargetType.PARAMETER, e.getName());
        return ResponseEntityBuilder.of(errorResponse, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<?> handleAnyOtherException(Exception e) {
        var callCtx = new RestContext();
        logException(callCtx, e);

        var errorResponse = ErrorResponseBuilder.of(callCtx, e, TargetType.URI, callCtx.getRequest().getRequestURI());

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (e instanceof HttpRequestMethodNotSupportedException) {
            httpStatus = HttpStatus.METHOD_NOT_ALLOWED;
        }

        return ResponseEntityBuilder.of(errorResponse, httpStatus);
    }

    private static void logException(CallContext callCtx, Throwable e) {
        callCtx.getLogger().error(callCtx.getLogRef(), e);
    }

}
