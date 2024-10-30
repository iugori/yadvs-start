package ro.iugori.yadvs.web.rest.config;

import jakarta.persistence.EntityNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.NotImplementedException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.servlet.NoHandlerFoundException;
import ro.iugori.yadvs.web.rest.util.ErrorResponseUtil;
import ro.iugori.yadvs.web.rest.util.ResponseEntityUtil;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.error.YadvsRestException;
import ro.iugori.yadvs.web.rest.model.RestContext;

import java.util.NoSuchElementException;
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
        if (exCause instanceof CheckException checkException) {
            httpStatus = HttpStatus.BAD_REQUEST;
            for (var error : checkException.getErrors()) {
                if (error.codeAsInt() == ErrorCode.RESOURCE_CONFLICT.code) {
                    httpStatus = HttpStatus.CONFLICT;
                    break;
                }
            }
        } else if (exCause instanceof MethodArgumentNotValidException) {
            httpStatus = HttpStatus.BAD_REQUEST;
        } else if (exCause instanceof HttpMessageNotReadableException) {
            httpStatus = HttpStatus.BAD_REQUEST;
        } else if (exCause instanceof MissingServletRequestParameterException) {
            httpStatus = HttpStatus.BAD_REQUEST;
        } else if (exCause instanceof NoSuchElementException) {
            httpStatus = HttpStatus.NOT_FOUND;
        } else if (exCause instanceof NoHandlerFoundException) {
            httpStatus = HttpStatus.NOT_FOUND;
        } else if (exCause instanceof EntityNotFoundException) {
            httpStatus = HttpStatus.NOT_FOUND;
        } else if (exCause instanceof HttpMediaTypeNotSupportedException) {
            httpStatus = HttpStatus.UNSUPPORTED_MEDIA_TYPE;
        } else if (exCause instanceof NotImplementedException) {
            httpStatus = HttpStatus.NOT_IMPLEMENTED;
        }

        if (ex.getHttpStatus() != null) {
            httpStatus = ex.getHttpStatus();
        }

        var errorResponse = ErrorResponseUtil.of(ex);
        return ResponseEntityUtil.of(errorResponse, httpStatus);
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<?> handleMethodArgumentTypeMismatchException(MethodArgumentTypeMismatchException e) {
        var callCtx = RestContext.fromRequestContextHolder();
        logException(callCtx, e);
        var errorResponse = ErrorResponseUtil.of(callCtx, e, TargetType.PARAMETER, e.getName());
        return ResponseEntityUtil.of(errorResponse, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<?> handleAnyOtherException(Exception e) {
        var callCtx = RestContext.fromRequestContextHolder();
        logException(callCtx, e);

        var errorResponse = ErrorResponseUtil.of(callCtx, e, TargetType.URI, callCtx.getRequest().getRequestURI());

        var httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
        if (e instanceof HttpRequestMethodNotSupportedException) {
            httpStatus = HttpStatus.METHOD_NOT_ALLOWED;
        } else if (e instanceof HttpMediaTypeNotSupportedException) {
            httpStatus = HttpStatus.UNSUPPORTED_MEDIA_TYPE;
        }

        return ResponseEntityUtil.of(errorResponse, httpStatus);
    }

    private static void logException(CallContext callCtx, Throwable e) {
        callCtx.getLogger().error(callCtx.getLogRef(), e);
    }

}
