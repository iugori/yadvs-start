package ro.iugori.yadvs.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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

@RestControllerAdvice
@Slf4j
public class RestExceptionHandler {

    @ExceptionHandler(YadvsRestException.class)
    public ResponseEntity<?> handleYadvsRestException(YadvsRestException ye) {
        var callCtx = ye.getCallCtx();

        var errorResponse = ErrorResponseBuilder.of(ye);

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (ye.getCause() instanceof CheckException ce) {
            logException(callCtx, ce);
            httpStatus = HttpStatus.BAD_REQUEST;
        } else {
            logException(callCtx, ye);
        }

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

    private static void logException(CallContext callCtx, Exception e) {
        callCtx.getLogger().error(callCtx.getLogRef(), e);
    }


}
