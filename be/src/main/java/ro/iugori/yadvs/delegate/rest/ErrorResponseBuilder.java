package ro.iugori.yadvs.delegate.rest;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.metadata.ConstraintDescriptor;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.hateoas.Link;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.ctx.RestContext;
import ro.iugori.yadvs.model.error.*;
import ro.iugori.yadvs.model.rest.ErrorResponse;

import java.util.Optional;
import java.util.Set;

public class ErrorResponseBuilder {

    private static ErrorResponse of(CallContext callCtx) {
        var path = "N/A";
        if (callCtx instanceof RestContext restCtx && restCtx.getRequest() != null) {
            path = restCtx.getRequest().getRequestURI();
        }
        return new ErrorResponse(callCtx.getLogRef(), callCtx.getTimeRef(), path);
    }

    private static ErrorResponse of(CallContext callCtx, Exception e) {
        var error = new ErrorModel();
        error.setCode(errorCodeOf(e));
        error.setMessage(e.getMessage());
        var errors = of(callCtx);
        errors.add(error);
        return errors;
    }

    public static ErrorResponse of(YadvsRestException yadvsEx) {
        var exCause = yadvsEx.getCause();

        var errors = of(yadvsEx.getCallCtx());

        if (exCause instanceof CheckException checkEx) {
            var swaggerUrl = getSwaggerUrl(((RestContext) yadvsEx.getCallCtx()).getRequest());
            for (var error : checkEx.getErrors()) {
                var errorClone = SerializationUtils.clone(error);
                errorClone.add(Link.of(swaggerUrl, "swagger"));
                errors.add(errorClone);
            }
        } else {
            var ex = exCause == null ? yadvsEx : exCause;
            var error = new ErrorModel();
            error.setCode(errorCodeOf(ex));
            error.setMessage(ex.getMessage());
            errors.add(error);
        }

        return errors;
    }

    public static ErrorResponse of(RestContext restCtx, Exception e, TargetType targetType, String targetName) {
        var errors = of(restCtx, e);
        var error = errors.getError(0);
        if (error.getLinks().isEmpty()) {
            error.add(Link.of(getSwaggerUrl(restCtx.getRequest()), "swagger"));
        }
        error.setTarget(targetType, targetName);
        return errors;
    }

    public static <T> ErrorResponse of(RestContext restCtx, Set<ConstraintViolation<T>> validationResult) {
        var errors = of(restCtx);
        validationResult.forEach(constraint -> {
            var error = errorModelOf(constraint);
            error.add(Link.of(getSwaggerUrl(restCtx.getRequest()), "swagger"));
            errors.add(error);
        });
        return errors;
    }

    public static <T> ErrorModel errorModelOf(ConstraintViolation<T> constraint) {
        var error = new ErrorModel();
        error.setCode(errorCodeOf(constraint.getConstraintDescriptor()));
        error.setMessage(constraint.getMessage());
        error.setTarget(TargetType.FIELD, String.valueOf(constraint.getPropertyPath()));
        return error;
    }

    private static ErrorCode errorCodeOf(ConstraintDescriptor<?> descriptor) {
        var nameHint = descriptor.getAnnotation().annotationType().getSimpleName();
        return switch (nameHint) {
            case "NotNull" -> ErrorCode.NOT_NULL;
            case "NotEmpty" -> ErrorCode.NOT_EMPTY;
            default -> ErrorCode.GENERIC;
        };
    }

    private static ErrorCode errorCodeOf(Throwable ex) {
        return switch (ex) {
            case NumberFormatException ignored -> ErrorCode.TYPE_CONVERSION;
            case HttpRequestMethodNotSupportedException ignored -> ErrorCode.API_ERROR;
            case HttpMediaTypeNotSupportedException ignored -> ErrorCode.API_ERROR;
            case MethodArgumentTypeMismatchException ignored -> ErrorCode.TYPE_CONVERSION;
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
