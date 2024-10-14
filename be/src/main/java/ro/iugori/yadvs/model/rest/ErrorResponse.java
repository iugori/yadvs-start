package ro.iugori.yadvs.model.rest;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@RequiredArgsConstructor
public class ErrorResponse {

    @Getter
    private final String trace;

    @Getter
    private final List<ErrorModel> errors = new ArrayList<>();

    public void add(ErrorModel error) {
        errors.add(error);
    }

}
