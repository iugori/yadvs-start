package ro.iugori.yadvs.delegates.rest;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.metadata.ConstraintDescriptor;
import ro.iugori.yadvs.model.rest.ErrorCode;
import ro.iugori.yadvs.model.rest.ErrorModel;
import ro.iugori.yadvs.model.rest.TargetType;

public class ErrorBuilder {

    public static ErrorModel of(ConstraintViolation<?> constraint) {
        var error = new ErrorModel();
        error.setCode(toErrorCode(constraint.getConstraintDescriptor()));
        error.setMessage(constraint.getMessage());
        error.setTarget(TargetType.FIELD, constraint.getPropertyPath().toString());
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

}
