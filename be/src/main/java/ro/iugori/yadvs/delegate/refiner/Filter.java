package ro.iugori.yadvs.delegate.refiner;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Filter implements Iterable<FilterPredicate> {

    private final List<FilterPredicate> predicates = new ArrayList<>();


    @Override
    public Iterator<FilterPredicate> iterator() {
        return predicates.iterator();
    }

    public Filter add(FilterPredicate predicate) {
        predicates.add(predicate);
        return this;
    }

}
