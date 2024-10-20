package ro.iugori.yadvs.model.error;

public enum ErrorCode {

    GENERIC(1000),
    API_ERROR(1010),
    NOT_ALLOWED(1020),
    NOT_NULL(2000),
    NOT_EMPTY(2001),

    TYPE_CONVERSION(3000),
    FIELD_NAME(3010),

    RESOURCE_CONFLICT(3100),
    PROJECTION_CRITERIA(3210),
    SELECTION_CRITERIA(3220),
    SORTING_CRITERIA(3230),
    PAGINATION_CRITERIA(3240),

    ;

    public final int code;

    ErrorCode(int code) {
        this.code = code;
    }

}
