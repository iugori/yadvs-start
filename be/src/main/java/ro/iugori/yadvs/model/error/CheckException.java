package ro.iugori.yadvs.model.error;

import lombok.Getter;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class CheckException extends RuntimeException {

    @Getter
    private final List<ErrorModel> errors;

    public CheckException(List<ErrorModel> errors) {
        this.errors = Collections.unmodifiableList(errors);
    }

    public CheckException(ErrorModel... errors) {
        this.errors = Arrays.asList(errors);
    }

    @Override
    public String getMessage() {
        return errors.stream()
                .map(ErrorModel::toString)
                .collect(Collectors.joining("\n", "Failed checks:\n", ""));
    }

}
