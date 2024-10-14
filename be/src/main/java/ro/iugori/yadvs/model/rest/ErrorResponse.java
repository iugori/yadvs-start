package ro.iugori.yadvs.model.rest;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public class ErrorResponse {

    @Getter
    private final List<ErrorModel> errors = new ArrayList<>();

    public void add(ErrorModel error) {
        errors.add(error);
    }

}
