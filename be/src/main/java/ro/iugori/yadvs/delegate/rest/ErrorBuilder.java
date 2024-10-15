package ro.iugori.yadvs.delegate.rest;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.metadata.ConstraintDescriptor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import ro.iugori.yadvs.delegate.ctx.CallContext;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.model.rest.ErrorCode;
import ro.iugori.yadvs.model.rest.ErrorModel;
import ro.iugori.yadvs.model.rest.ErrorResponse;
import ro.iugori.yadvs.model.rest.TargetType;

import java.util.Optional;
import java.util.Set;

public class ErrorBuilder {

    public static ErrorResponse responseOf(CallContext callCtx) {
        var path = "N/A";
        if (callCtx instanceof RestContext restCtx && restCtx.getRequest() != null) {
            path = restCtx.getRequest().getRequestURI();
        }
        return new ErrorResponse(callCtx.getTraceId(), callCtx.getTraceTs(), path);
    }

    public static ErrorResponse responseOf(CallContext callCtx, Exception e) {
        var error = new ErrorModel();
        error.setCode(toErrorCode(e));
        error.setMessage(e.getMessage());

        if (e instanceof HttpRequestMethodNotSupportedException ex) {
            error.setMoreInfo(String.valueOf(ex.getBody().getType()));
        }

        var errors = responseOf(callCtx);
        errors.add(error);
        return errors;
    }

    public static ErrorResponse responseOf(RestContext restCtx, Exception e, TargetType targetType, String targetName) {
        var errors = responseOf(restCtx, e);
        var error = errors.getErrors().get(0);
        if (error.getMoreInfo() == null) {
            error.setMoreInfo(getSwaggerUrl(restCtx.getRequest()));
        }
        error.setTarget(targetType, targetName);
        return errors;
    }

    public static <T> ErrorResponse responseOf(RestContext restCtx, Set<ConstraintViolation<T>> validationResult) {
        var errors = responseOf(restCtx);
        validationResult.forEach(constraint -> {
            var error = new ErrorModel();
            error.setCode(toErrorCode(constraint.getConstraintDescriptor()));
            error.setMessage(constraint.getMessage());
            error.setMoreInfo(getSwaggerUrl(restCtx.getRequest()));
            error.setTarget(TargetType.FIELD, String.valueOf(constraint.getPropertyPath()));
            errors.add(error);
        });
        return errors;
    }

    private static ErrorCode toErrorCode(ConstraintDescriptor<?> descriptor) {
        var nameHint = descriptor.getAnnotation().annotationType().getSimpleName();
        return switch (nameHint) {
            case "NotNull" -> ErrorCode.NOT_NULL;
            case "NotEmpty" -> ErrorCode.NOT_EMPTY;
            default -> ErrorCode.GENERIC;
        };
    }

    private static ErrorCode toErrorCode(Exception ex) {
        return switch (ex) {
            case NumberFormatException ignored -> ErrorCode.CONVERSION_ERROR;
            case HttpRequestMethodNotSupportedException ignored -> ErrorCode.API_ERROR;
            case MethodArgumentTypeMismatchException ignored -> ErrorCode.CONVERSION_ERROR;
            default -> ErrorCode.GENERIC;
        };
    }

    private static String getSwaggerUrl(HttpServletRequest request) {
        var url = new StringBuilder(String.format("%s://%s:%d",
                request.getScheme(), request.getServerName(), request.getServerPort()));
        Optional.ofNullable(request.getContextPath())
                .filter(StringUtils::isNotEmpty)
                .map(path -> url.append("/").append(path));
        url.append("/swagger-ui.html");
        return url.toString();
    }

}
