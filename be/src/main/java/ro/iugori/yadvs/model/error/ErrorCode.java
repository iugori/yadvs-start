package ro.iugori.yadvs.model.error;

import jakarta.validation.metadata.ConstraintDescriptor;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

public enum ErrorCode {

    // System errors
    GENERIC(1000),
    API_ERROR(1010),

    // Validation errors
    NOT_NULL(2000),
    NOT_EMPTY(2001),
    NOT_ALLOWED(2100),
    TYPE_CONVERSION(2101),
    RESOURCE_CONFLICT(2102),

    // Query parsing errors
    PROJECTION_CRITERIA(3210),
    SELECTION_CRITERIA(3220),
    SORTING_CRITERIA(3230),
    PAGINATION_CRITERIA(3240),

    ;

    public final int code;

    ErrorCode(int code) {
        this.code = code;
    }

    public static ErrorCode of(Throwable ex) {
        return switch (ex) {
            case NumberFormatException ignored -> ErrorCode.TYPE_CONVERSION;
            case HttpRequestMethodNotSupportedException ignored -> ErrorCode.API_ERROR;
            case HttpMediaTypeNotSupportedException ignored -> ErrorCode.API_ERROR;
            case MethodArgumentTypeMismatchException ignored -> ErrorCode.TYPE_CONVERSION;
            default -> ErrorCode.GENERIC;
        };
    }

    public static ErrorCode of(ConstraintDescriptor<?> descriptor) {
        var nameHint = descriptor.getAnnotation().annotationType().getSimpleName();
        return switch (nameHint) {
            case "NotNull" -> ErrorCode.NOT_NULL;
            case "NotEmpty" -> ErrorCode.NOT_EMPTY;
            default -> ErrorCode.GENERIC;
        };
    }


}
