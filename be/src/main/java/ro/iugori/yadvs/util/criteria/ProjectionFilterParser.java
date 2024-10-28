package ro.iugori.yadvs.util.criteria;

import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.model.criteria.ProjectionFilter;
import ro.iugori.yadvs.util.text.TextUtil;

import java.text.ParseException;

public class ProjectionFilterParser {

    public static ProjectionFilter parse(String script) throws ParseException {
        var projector = new ProjectionFilter();

        var fields = StringUtils.trimToNull(script);
        if (fields == null) {
            return projector;
        }

        for (var field : fields.split(",")) {
            field = StringUtils.trimToNull(field);
            if (field == null) {
                continue;
            }
            if (TextUtil.isValidIdentifier(field)) {
                projector.add(field);
            } else {
                throw new ParseException(
                        String.format("Cannot use projection field `%s' (must be a valid Java identifier).", field),
                        script.indexOf(field));
            }
        }

        return projector;
    }

}
