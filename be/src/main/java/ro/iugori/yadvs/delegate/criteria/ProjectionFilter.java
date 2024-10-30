package ro.iugori.yadvs.delegate.criteria;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

public class ProjectionFilter implements Iterable<String> {

    private final List<String> fields = new ArrayList<>();

    public static ProjectionFilter parse(String script) throws ParseException {
        return ProjectionFilterParser.parse(script);
    }

    @Override
    public Iterator<String> iterator() {
        return fields.iterator();
    }

    public Stream<String> stream() {
        return fields.stream();
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
