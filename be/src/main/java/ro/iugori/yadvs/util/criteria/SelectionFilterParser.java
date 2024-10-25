package ro.iugori.yadvs.util.criteria;

import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.model.criteria.SelectionFilter;
import ro.iugori.yadvs.util.rest.RestApi;
import ro.iugori.yadvs.util.text.TextUtil;

import java.text.ParseException;

public class SelectionFilterParser {

    public static SelectionFilter.Predicate parse(String script) throws ParseException {
        script = StringUtils.trimToEmpty(script);
        var scriptValue = script.split("=", 2);
        if (scriptValue.length != 2) {
            throw new ParseException("Cannot parse selection predicate without name and value (`=' is missing).", 0);
        }
        return parse(scriptValue[0], scriptValue[1]);
    }

    public static SelectionFilter.Predicate parse(String script, Object value) throws ParseException {
        var name = StringUtils.trimToNull(script);
        if (name == null) {
            throw new ParseException("Cannot parse selection predicate without name.", 0);
        }
        if (value == null) {
            throw new ParseException("Cannot parse selection predicate with null value.", 0);
        }

        var op = SelectionFilter.Operator.EQ;
        var opStartIdx = name.lastIndexOf(RestApi.RESERVED_PARAM);
        if (opStartIdx > 0) {
            var opScript = name.substring(opStartIdx + 1);
            op = SelectionFilter.Operator.parse(opScript);

            name = StringUtils.trimToNull(name.substring(0, opStartIdx));
            if (name == null) {
                throw new ParseException("Cannot parse selection predicate without name.", 0);
            }
        }

        if (!TextUtil.isValidIdentifier(name)) {
            throw new ParseException(
                    String.format("Cannot parse selection predicate with field `%s' (must be a valid Java identifier).", name),
                    0);
        }

        return new SelectionFilter.Predicate(name, op, value);
    }

}
