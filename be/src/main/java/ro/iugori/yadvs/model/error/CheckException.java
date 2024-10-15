package ro.iugori.yadvs.model.error;

import lombok.Getter;
import ro.iugori.yadvs.delegate.ctx.CallContext;

import java.util.Arrays;
import java.util.List;

public class CheckException extends YadvsException {

    @Getter
    private final List<ErrorModel> errors;

    public CheckException(CallContext callCtx, ErrorModel... errors) {
        super(callCtx);
        this.errors = Arrays.asList(errors);
    }

    public CheckException(CallContext callCtx, Throwable cause) {
        super(callCtx, cause);
        errors = List.of();
    }


}
