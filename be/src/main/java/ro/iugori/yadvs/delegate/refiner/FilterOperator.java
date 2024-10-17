package ro.iugori.yadvs.delegate.refiner;

import java.util.List;

public enum FilterOperator {

    LT(List.of("<", "lt")),
    LTE(List.of("<=", "lte")),
    EQ(List.of("==", "eq")),
    NE(List.of("!=", "ne", "neq")),
    GTE(List.of(">", "gt")),
    GT(List.of(">=", "gte")),
    IN(List.of("in")),
    ;

    public final List<String> hints;

    FilterOperator(List<String> hints) {
        this.hints = hints;
    }
}
