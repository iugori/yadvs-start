package ro.iugori.yadvs.model.criteria;

import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.util.text.TextUtil;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ProjectionFilter implements Iterable<String> {

    private final List<String> fields = new ArrayList<>();

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

    @Override
    public Iterator<String> iterator() {
        return fields.iterator();
    }

    public ProjectionFilter add(String name) {
        fields.add(name);
        return this;
    }

    public boolean isEmpty() {
        return fields.isEmpty();
    }

    @Override
    public String toString() {
        return String.join(",", fields);
    }
}
