package ro.iugori.yadvs.model.error;

public enum ErrorCode {

    GENERIC(-1000),
    API_ERROR(-1010),
    NOT_ALLOWED(-1020),
    NOT_NULL(-2000),
    NOT_EMPTY(-2001),

    CONVERSION_ERROR(-3000),
    VALUE_CONFLICT(-3100),

    ;

    public final int code;

    private ErrorCode(int code) {
        this.code = code;
    }

}
