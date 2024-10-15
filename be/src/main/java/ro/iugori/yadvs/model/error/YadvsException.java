package ro.iugori.yadvs.model.error;

import lombok.Getter;
import ro.iugori.yadvs.delegate.ctx.CallContext;

public class YadvsException extends RuntimeException {

    @Getter
    private final CallContext callCtx;

    public YadvsException(CallContext callCtx) {
        this.callCtx = callCtx;
    }

    public YadvsException(CallContext callCtx, String message) {
        super(message);
        this.callCtx = callCtx;
    }

    public YadvsException(CallContext callCtx, String message, Throwable cause) {
        super(message, cause);
        this.callCtx = callCtx;
    }

    public YadvsException(CallContext callCtx, Throwable cause) {
        super(cause);
        this.callCtx = callCtx;
    }

}
