package ro.iugori.yadvs.model.rest;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.springframework.hateoas.RepresentationModel;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.util.TimeUtil;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class ErrorResponse extends RepresentationModel<ErrorResponse> {

    @Getter
    @Setter
    private int status;

    @Getter
    private final String path;

    private final String timeRef;

    private final String logRef;

    private final List<ErrorModel> errors = new ArrayList<>();


    public ErrorResponse(String logRef, LocalDateTime timeRef, String path) {
        this.logRef = logRef;
        this.timeRef = TimeUtil.toIsoDefaultZoneTs(timeRef);
        this.path = path;
    }

    public void add(ErrorModel error) {
        errors.add(error);
    }

    public ErrorModel getError(int index) {
        return errors.get(index);
    }

    public boolean hasNoErrors() {
        return errors.isEmpty();
    }

    @JsonProperty("timeref")
    public String getTimeRef() {
        return timeRef;
    }

    @JsonProperty("logref")
    public String getLogRef() {
        return logRef;
    }

    @JsonProperty("_embedded")
    public Object getErrors() {
        @AllArgsConstructor
        @Getter
        class Wrapper {
            private final List<ErrorModel> errors;
        }
        return new Wrapper(errors);
    }

}
