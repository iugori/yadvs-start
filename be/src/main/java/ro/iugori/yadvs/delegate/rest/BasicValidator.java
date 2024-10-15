package ro.iugori.yadvs.delegate.rest;

import jakarta.validation.Validator;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.model.rest.ErrorResponse;

import java.util.Optional;

public class BasicValidator {

    public static Optional<ErrorResponse> validateBean(RestContext restCtx, Validator validator, Object bean) {
        var validationResult = validator.validate(bean);
        if (!validationResult.isEmpty()) {
            return Optional.of(ErrorBuilder.responseOf(restCtx, validationResult));
        }
        return Optional.empty();
    }

}
