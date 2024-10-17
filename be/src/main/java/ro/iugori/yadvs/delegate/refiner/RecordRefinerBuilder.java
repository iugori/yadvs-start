package ro.iugori.yadvs.delegate.refiner;

import java.text.ParseException;

public class RecordRefinerBuilder {


    private Filter filter;
    private Sorter sorter;

    private Integer limit;
    private Integer page;
    private Integer offset;


    public RecordRefiner build() {
        return new RecordRefiner(filter, sorter, limit, page);
    }

    public RecordRefinerBuilder filterPredicate(String filterPredicate) {
        if (filter == null) {
            filter = new Filter();
        }
        filter.add(FilterPredicate.parse(filterPredicate));
        return this;
    }

    public RecordRefinerBuilder sorter(String sorter) {
        sorter = Sorter.parse(sorter);
        return this;
    }

    public RecordRefinerBuilder sortField(String sortField) throws ParseException {
        if (sorter == null) {
            sorter = new Sorter();
        }
        sorter.add(SortField.parse(sortField));
        return this;
    }


}
