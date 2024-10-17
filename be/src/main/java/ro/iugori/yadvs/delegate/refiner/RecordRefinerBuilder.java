package ro.iugori.yadvs.delegate.refiner;

import org.apache.commons.lang3.StringUtils;

import java.text.ParseException;

public class RecordRefinerBuilder {


    private Projector projector;
    private Selector selector;
    private Sorter sorter;

    private Integer pageNo;
    private Integer pageSize;


    public RecordRefiner build() {
        if (pageNo != null && pageSize != null) {
            var offset = RecordRefiner.offsetFromPage(pageNo, pageSize);
            return new RecordRefiner(projector, selector, sorter, offset, pageSize);
        }
        return new RecordRefiner(projector, selector, sorter, null, pageSize);
    }

    public void select(String script, Object value) throws ParseException {
        if (selector == null) {
            selector = new Selector();
        }
        selector.add(Selector.Predicate.parse(script, value));
    }

    public RecordRefinerBuilder project(String script) throws ParseException {
        this.projector = Projector.parse(script);
        return this;
    }


    public RecordRefinerBuilder sort(String script) throws ParseException {
        this.sorter = Sorter.parse(script);
        return this;
    }

    public RecordRefinerBuilder paginate(String pageNo, String pageSize) {
        pageNo = StringUtils.trimToNull(pageNo);
        this.pageNo = pageNo == null ? null : Integer.parseInt(pageNo);
        pageSize = StringUtils.trimToNull(pageSize);
        this.pageSize = pageSize == null ? null : Integer.parseInt(pageSize);
        return this;
    }


}
