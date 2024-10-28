package ro.iugori.yadvs.util.criteria;

import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.model.criteria.SortOrder;
import ro.iugori.yadvs.util.text.TextUtil;

import java.text.ParseException;

public class SortOrderParser {

    public static SortOrder.Field parse(String script) throws ParseException {
        var name = StringUtils.trimToNull(script);
        if (name == null) {
            throw new ParseException("Cannot parse sort field from empty string.", 0);
        }

        var order = SortOrder.Direction.ASC;
        if (name.startsWith("-")) {
            order = SortOrder.Direction.DESC;
            name = name.substring(1);
        } else if (name.startsWith("+")) {
            name = name.substring(1);
        }

        name = StringUtils.trimToNull(name);
        if (name == null) {
            throw new ParseException("Cannot parse sort field without name.", script.length() - 1);
        }
        if (!TextUtil.isValidIdentifier(name)) {
            throw new ParseException(
                    String.format("Cannot parse sort field `%s' (must be a valid Java identifier).", name),
                    script.indexOf(name));
        }

        return new SortOrder.Field(name, order);
    }

}
