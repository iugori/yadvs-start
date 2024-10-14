package ro.iugori.yadvs.delegate.rest;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.metadata.ConstraintDescriptor;
import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.model.rest.ErrorCode;
import ro.iugori.yadvs.model.rest.ErrorModel;
import ro.iugori.yadvs.model.rest.TargetType;

import java.util.Optional;

public class ErrorBuilder {

    public static ErrorModel of(HttpServletRequest request, ConstraintViolation<?> constraint) {
        var error = new ErrorModel();
        error.setCode(toErrorCode(constraint.getConstraintDescriptor()));
        error.setMessage(constraint.getMessage());
        error.setMoreInfo(getSwaggerUrl(request));
        error.setTarget(TargetType.FIELD, String.valueOf(constraint.getPropertyPath()));
        return error;
    }


    public static ErrorCode toErrorCode(ConstraintDescriptor<?> descriptor) {
        var nameHint = descriptor.getAnnotation().annotationType().getSimpleName();
        return switch (nameHint) {
            case "NotNull" -> ErrorCode.NOT_NULL;
            case "NotEmpty" -> ErrorCode.NOT_EMPTY;
            default -> ErrorCode.GENERIC;
        };
    }

    public static String getSwaggerUrl(HttpServletRequest request) {
        var url = new StringBuilder(String.format("%s://%s:%d",
                request.getScheme(), request.getServerName(), request.getServerPort()));
        Optional.ofNullable(request.getContextPath())
                .filter(StringUtils::isNotEmpty)
                .map(path -> url.append("/").append(path));
        url.append("/swagger-ui.html");
        return url.toString();
    }

}
