package ro.iugori.yadvs.model.rest;

public enum ErrorCode {

    GENERIC(-1000),
    API_ERROR(-1001),
    NOT_NULL(-2000),
    NOT_EMPTY(-2001),

    CONVERSION_ERROR(-3000),

    ;

    public final int code;

    private ErrorCode(int code) {
        this.code = code;
    }

}
