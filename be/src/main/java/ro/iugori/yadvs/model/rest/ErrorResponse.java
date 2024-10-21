package ro.iugori.yadvs.model.rest;

import com.fasterxml.jackson.annotation.JsonInclude;
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

@JsonInclude(JsonInclude.Include.NON_NULL)
public class ErrorResponse extends RepresentationModel<ErrorResponse> {

    @Getter
    @Setter
    private String type; // RFC 9457

    @Getter
    @Setter
    private int status; // RFC 9457

    @Getter
    @Setter
    private String title; // RFC 9457

    @Getter
    @Setter
    private String detail; // RFC 9457

    @Getter
    @Setter
    private String instance; // RFC 9457

    @Getter
    private final String path;

    @Getter
    private final String timestamp;

    private final String logRef;

    private final List<ErrorModel> errors = new ArrayList<>();


    public ErrorResponse(String logRef, LocalDateTime timestamp, String path) {
        this.logRef = logRef;
        this.timestamp = TimeUtil.toIsoDefaultZoneTs(timestamp);
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
