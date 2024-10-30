package ro.iugori.yadvs.delegate.criteria;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.util.rest.RestApi;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

public class SelectionFilter implements Iterable<SelectionFilter.Predicate> {

    public enum Operator {

        EQ(List.of("eq", "==")),
        IS(List.of("is")),
        LT(List.of("lt", "<")),
        LE(List.of("le", "<=", "lte")),
        NE(List.of("ne", "!=", "neq")),
        GE(List.of("ge", ">=", "gte")),
        GT(List.of("gt", ">")),
        IN(List.of("in")),
        LIKE(List.of("like")),
        UNLIKE(List.of("unlike")),
        ;

        public final List<String> hints;

        Operator(List<String> hints) {
            this.hints = hints;
        }

        public static Operator parse(String script) throws ParseException {
            var hint = StringUtils.trimToEmpty(script).toLowerCase(Locale.ROOT);
            for (var op : values()) {
                if (op.hints.contains(hint)) {
                    return op;
                }
            }
            throw new ParseException(String.format("Cannot parse selection predicate operator `%s'.", script), 0);
        }

    }

    @AllArgsConstructor
    @Getter
    @Setter
    public static class Predicate {

        private String name;
        private Operator op;
        private Object value;

        public static Predicate parse(String script) throws ParseException {
            return SelectionFilterParser.parse(script);
        }

        public static Predicate parse(String script, Object value) throws ParseException {
            return SelectionFilterParser.parse(script, value);
        }

        @Override
        public String toString() {
            var rep = new StringBuilder(name);
            if (op != Operator.EQ) {
                rep.append(RestApi.RESERVED_PARAM).append(op.hints.get(0));
            }
            return rep.append("=").append(value).toString();
        }

    }

    private final List<Predicate> predicates = new ArrayList<>();

    @Override
    public Iterator<Predicate> iterator() {
        return predicates.iterator();
    }

    public Stream<Predicate> stream() {
        return predicates.stream();
    }

    public SelectionFilter add(Predicate predicate) {
        predicates.add(predicate);
        return this;
    }

    public boolean isEmpty() {
        return predicates.isEmpty();
    }


}
