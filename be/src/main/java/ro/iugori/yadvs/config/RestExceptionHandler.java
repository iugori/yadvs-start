package ro.iugori.yadvs.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
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
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.error.YadvsRestException;
import ro.iugori.yadvs.model.rest.ErrorResponse;
import ro.iugori.yadvs.web.RestApi;

@RestControllerAdvice
@Slf4j
public class RestExceptionHandler {

    @ExceptionHandler(YadvsRestException.class)
    public ResponseEntity<Object> handleYadvsRestException(YadvsRestException ye) {
        var callCtx = ye.getCallCtx();

        var errorResponse = ErrorResponseBuilder.errorsOf(ye);

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (ye.getCause() instanceof CheckException ce) {
            logException(callCtx, ce);
            httpStatus = HttpStatus.BAD_REQUEST;
        } else {
            logException(callCtx, ye);
        }

        return buildResponseEntity(errorResponse, httpStatus);
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<Object> handleMethodArgumentTypeMismatchException(MethodArgumentTypeMismatchException e) {
        var callCtx = new RestContext();
        logException(callCtx, e);
        var errorResponse = ErrorResponseBuilder.errorsOf(callCtx, e, TargetType.PARAMETER, e.getName());
        return buildResponseEntity(errorResponse, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<Object> handleAnyOtherException(Exception e) {
        var callCtx = new RestContext();
        logException(callCtx, e);

        var errorResponse = ErrorResponseBuilder.errorsOf(callCtx, e, TargetType.URI, callCtx.getRequest().getRequestURI());

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (e instanceof HttpRequestMethodNotSupportedException) {
            httpStatus = HttpStatus.METHOD_NOT_ALLOWED;
        }

        return buildResponseEntity(errorResponse, httpStatus);
    }

    private static void logException(CallContext callCtx, Exception e) {
        callCtx.getLogger().error(callCtx.getLogRef(), e);
    }

    private static ResponseEntity<Object> buildResponseEntity(ErrorResponse body, HttpStatus status) {
        body.setStatus(status.value());
        var headers = new HttpHeaders();
        headers.add(RestApi.Header.X_CORRELATION_ID, body.getLogRef());
        return new ResponseEntity<>(body, headers, status);
    }

}
