package ro.iugori.yadvs.delegate.refiner;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Sorter implements Iterable<SortField> {

    private final List<SortField> fields = new ArrayList<>();

    public static String parse(String source) {
        return null;
    }


    @Override
    public Iterator<SortField> iterator() {
        return fields.iterator();
    }

    public Sorter add(SortField newField) {
        for (var field : fields) {
            if (field.getName().equals(newField.getName())) {
                if (field.getOrder().equals(newField.getOrder())) {
                    return this;
                } else {
                    throw new IllegalArgumentException(String.format("Both ASC and DSC ordering for `%s'", field.getName()));
                }
            }
        }
        fields.add(newField);
        return this;
    }

}
