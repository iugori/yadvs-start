package ro.iugori.yadvs.delegate.criteria;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.util.TextUtil;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

public class Selector implements Iterable<Selector.Predicate> {

    public enum Operator {

        LT(List.of("lt", "<")),
        LTE(List.of("lte", "<=")),
        EQ(List.of("eq", "==")),
        NE(List.of("ne", "!=", "neq")),
        GTE(List.of("gt", ">")),
        GT(List.of("gte", ">=")),
        IN(List.of("in")),
        LIKE(List.of("like")),
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
            script = StringUtils.trimToEmpty(script);
            var scriptValue = script.split("=", 2);
            if (scriptValue.length != 2) {
                throw new ParseException("Cannot parse selection predicate without name and value (`=' is missing).", 0);
            }
            return parse(scriptValue[0], scriptValue[1]);
        }

        public static Predicate parse(String script, Object value) throws ParseException {
            var name = StringUtils.trimToNull(script);
            if (name == null) {
                throw new ParseException("Cannot parse selection predicate without name.", 0);
            }
            if (value == null) {
                throw new ParseException("Cannot parse selection predicate with null value.", 0);
            }

            var op = Operator.EQ;
            var opStartIdx = name.indexOf("[");
            if (opStartIdx >= 0) {
                var opEndIdx = name.lastIndexOf("]");
                if (opEndIdx != name.length() - 1) {
                    throw new ParseException("Cannot parse selection predicate with invalid operation specifier.", name.length() - opEndIdx);
                }
                var opScript = name.substring(opStartIdx + 1, opEndIdx);
                op = Operator.parse(opScript);

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

            return new Predicate(name, op, value);
        }

        @Override
        public String toString() {
            var rep = new StringBuilder(name);
            if (op != Operator.EQ) {
                rep.append("[").append(op.hints.get(0)).append("]");
            }
            return rep.append("=").append(value).toString();
        }

    }

    private final List<Predicate> predicates = new ArrayList<>();

    @Override
    public Iterator<Predicate> iterator() {
        return predicates.iterator();
    }

    public Selector add(Predicate predicate) {
        predicates.add(predicate);
        return this;
    }

    public boolean isEmpty() {
        return predicates.isEmpty();
    }


}
