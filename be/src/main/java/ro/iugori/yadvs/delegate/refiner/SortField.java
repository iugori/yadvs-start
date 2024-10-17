package ro.iugori.yadvs.delegate.refiner;


import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.util.TextUtil;

import java.text.ParseException;

@AllArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
public class SortField {

    private String name;
    private SortOrder order;

    public static SortField parse(String source) throws ParseException {
        var name = StringUtils.trimToNull(source);
        if (name == null) {
            throw new ParseException(String.format("Cannot parse %s from empty string."
                    , SortField.class.getSimpleName()), 0);
        }
        var order = SortOrder.ASC;
        if (name.startsWith("-")) {
            order = SortOrder.DSC;
            name = name.substring(1);
        } else if (name.startsWith("+")) {
            name = name.substring(1);
        }
        name = StringUtils.trimToNull(name);
        if (name == null) {
            throw new ParseException(String.format("Cannot parse %s without name."
                    , SortField.class.getSimpleName()), source.length() - 1);
        }
        if (!TextUtil.isValidIdentifier(name)) {
            throw new ParseException(String.format("Cannot parse %s with name `%s' (must be a valid Java identifier)."
                    , SortField.class.getSimpleName(), name), source.indexOf(name));
        }
        return new SortField(name, order);
    }

    @Override
    public String toString() {
        return String.format("%s%s", order == SortOrder.ASC ? "+" : "-", name);
    }

}
