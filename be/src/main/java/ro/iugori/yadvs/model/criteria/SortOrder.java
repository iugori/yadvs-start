package ro.iugori.yadvs.model.criteria;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.util.criteria.SortOrderParser;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SortOrder implements Iterable<SortOrder.Field> {

    public enum Direction {

        ASC,
        DESC,

    }

    @AllArgsConstructor
    @Getter
    @Setter
    @EqualsAndHashCode
    public static class Field {

        private String name;
        private Direction direction;

        public static Field parse(String script) throws ParseException {
            return SortOrderParser.parse(script);
        }

        @Override
        public String toString() {
            return String.format("%s%s", direction == Direction.ASC ? "+" : "-", name);
        }

    }

    private final List<Field> fields = new ArrayList<>();

    public static SortOrder parse(String script) throws ParseException {
        var sorter = new SortOrder();

        var fields = StringUtils.trimToNull(script);
        if (fields == null) {
            return sorter;
        }

        for (var fieldScript : script.split(",")) {
            fieldScript = StringUtils.trimToNull(fieldScript);
            if (fieldScript == null) {
                continue;
            }
            sorter.add(Field.parse(fieldScript));
        }

        return sorter;
    }

    @Override
    public Iterator<Field> iterator() {
        return fields.iterator();
    }

    public Stream<Field> stream() {
        return fields.stream();
    }

    public SortOrder add(Field newField) {
        for (var field : fields) {
            if (field.getName().equals(newField.getName())) {
                if (field.getDirection().equals(newField.getDirection())) {
                    return this;
                } else {
                    throw new IllegalArgumentException(String.format("Both ASC and DSC ordering for `%s'", field.getName()));
                }
            }
        }
        fields.add(newField);
        return this;
    }

    public boolean isEmpty() {
        return fields.isEmpty();
    }

    @Override
    public String toString() {
        return fields.stream().map(Field::toString).collect(Collectors.joining(","));
    }

}
