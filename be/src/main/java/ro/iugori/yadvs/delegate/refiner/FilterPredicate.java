package ro.iugori.yadvs.delegate.refiner;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class FilterPredicate {

    private String name;
    private FilterOperator op;
    private Object value;

    public static FilterPredicate parse(String str) {
        return null;
    }

}
