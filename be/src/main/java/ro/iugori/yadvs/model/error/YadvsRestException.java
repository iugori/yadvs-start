package ro.iugori.yadvs.model.error;

import lombok.Getter;
import org.springframework.http.HttpStatus;
import ro.iugori.yadvs.model.ctx.CallContext;

public class YadvsRestException extends RuntimeException {

    @Getter
    private final CallContext callCtx;
    @Getter
    private final HttpStatus httpStatus;

    public YadvsRestException(CallContext callCtx, String message, HttpStatus httpStatus) {
        super(message);
        this.callCtx = callCtx;
        this.httpStatus = httpStatus;
    }

    public YadvsRestException(CallContext callCtx, Throwable cause, HttpStatus httpStatus) {
        super(cause);
        this.callCtx = callCtx;
        this.httpStatus = httpStatus;
    }

    public YadvsRestException(CallContext callCtx, Throwable cause) {
        this(callCtx, cause, null);
    }

}
