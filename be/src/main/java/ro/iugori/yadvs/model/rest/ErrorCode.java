package ro.iugori.yadvs.model.rest;

public enum ErrorCode {

    GENERIC(100),
    NOT_NULL(101),
    NOT_EMPTY(102),

    ;

    public final int code;


    private ErrorCode(int code) {
        this.code = code;
    }
}
