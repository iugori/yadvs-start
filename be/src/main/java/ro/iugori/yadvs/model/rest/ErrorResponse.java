package ro.iugori.yadvs.model.rest;

import lombok.Getter;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.util.TimeUtil;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class ErrorResponse {

    @Getter
    private final String path;

    @Getter
    private final String timestamp;

    @Getter
    private final String trace;


    public ErrorResponse(String trace, LocalDateTime timestamp, String path) {
        this.trace = trace;
        this.timestamp = TimeUtil.toIsoDefaultZoneTs(timestamp);
        this.path = path;
    }

    @Getter
    private final List<ErrorModel> errors = new ArrayList<>();

    public void add(ErrorModel error) {
        errors.add(error);
    }

}
