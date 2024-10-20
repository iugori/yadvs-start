package ro.iugori.yadvs.model.error;

public enum ErrorCode {

    // System errors
    GENERIC(1000),
    API_ERROR(1010),

    // Validation errors
    NOT_NULL(2000),
    NOT_EMPTY(2001),
    NOT_ALLOWED(2100),
    TYPE_CONVERSION(2101),
    RESOURCE_CONFLICT(2102),

    // Query parsing errors
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
