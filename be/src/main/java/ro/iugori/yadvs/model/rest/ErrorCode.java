package ro.iugori.yadvs.model.rest;

public enum ErrorCode {

    GENERIC(-100),
    NOT_NULL(-200),
    NOT_EMPTY(-201),

    ;

    public final int code;

    private ErrorCode(int code) {
        this.code = code;
    }

}
