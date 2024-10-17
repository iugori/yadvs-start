package ro.iugori.yadvs.model.error;

import lombok.Getter;
import ro.iugori.yadvs.delegate.ctx.CallContext;

public class YadvsRestException extends RuntimeException {

    @Getter
    private final CallContext callCtx;

    public YadvsRestException(CallContext callCtx) {
        this.callCtx = callCtx;
    }

    public YadvsRestException(CallContext callCtx, String message) {
        super(message);
        this.callCtx = callCtx;
    }

    public YadvsRestException(CallContext callCtx, String message, Throwable cause) {
        super(message, cause);
        this.callCtx = callCtx;
    }

    public YadvsRestException(CallContext callCtx, Throwable cause) {
        super(cause);
        this.callCtx = callCtx;
    }

}
