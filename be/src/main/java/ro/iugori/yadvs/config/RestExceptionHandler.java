package ro.iugori.yadvs.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import ro.iugori.yadvs.delegate.rest.ErrorResponseBuilder;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.ctx.RestContext;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.error.YadvsRestException;

@RestControllerAdvice
@Slf4j
public class RestExceptionHandler {

    @ExceptionHandler(YadvsRestException.class)
    public ResponseEntity<Object> handleYadvsRestException(YadvsRestException ye) {
        var callCtx = ye.getCallCtx();

        var errorResponse = ErrorResponseBuilder.responseOf(ye);

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (ye.getCause() instanceof CheckException ce) {
            logException(callCtx, ce);
            httpStatus = HttpStatus.BAD_REQUEST;
            for (var error : ce.getErrors()) {
                if (ErrorCode.NOT_ALLOWED.code == error.getCodeAsInt()) {
                    httpStatus = HttpStatus.FORBIDDEN;
                    break;
                }
            }
        } else {
            logException(callCtx, ye);
        }

        return new ResponseEntity<>(errorResponse, httpStatus);
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<Object> handleMethodArgumentTypeMismatchException(MethodArgumentTypeMismatchException e) {
        var callCtx = new RestContext();
        logException(callCtx, e);
        var errorResponse = ErrorResponseBuilder.responseOf(callCtx, e, TargetType.PARAMETER, e.getName());
        return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<Object> handleAnyOtherException(Exception e) {
        var callCtx = new RestContext();
        logException(callCtx, e);

        var errorResponse = ErrorResponseBuilder.responseOf(callCtx, e, TargetType.URI, callCtx.getRequest().getRequestURI());

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (e instanceof HttpRequestMethodNotSupportedException) {
            httpStatus = HttpStatus.METHOD_NOT_ALLOWED;
        }

        return new ResponseEntity<>(errorResponse, httpStatus);
    }

    private static void logException(CallContext callCtx, Exception e) {
        callCtx.getLogger().error(callCtx.getTraceId(), e);
    }

}
