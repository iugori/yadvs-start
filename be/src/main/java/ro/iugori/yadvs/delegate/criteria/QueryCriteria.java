package ro.iugori.yadvs.delegate.criteria;

import org.apache.commons.lang3.StringUtils;

import java.text.ParseException;

public record QueryCriteria(Projector projectCriteria, Selector selectCriteria, Sorter sortCriteria, Integer offset,
                            Integer limit) {

    public static Builder builder() {
        return new Builder();
    }

    public static int pageFromOffset(int offset, int pageSize) {
        if (offset < 0 || pageSize <= 0) {
            throw new IllegalArgumentException("Offset and pageSize must be non-negative.");
        }
        return offset / pageSize + 1;
    }

    public static int offsetFromPage(int pageNo, int pageSize) {
        if (pageNo <= 0 || pageSize <= 0) {
            throw new IllegalArgumentException("Page and pageSize must be positive.");
        }
        return (pageNo - 1) * pageSize;
    }

    public boolean isEmpty() {
        return (projectCriteria == null || projectCriteria.isEmpty())
                && (selectCriteria == null || selectCriteria.isEmpty())
                && (sortCriteria == null || sortCriteria.isEmpty())
                && offset == null
                && limit == null;
    }

    public static class Builder {

        private Projector projector;
        private Selector selector;
        private Sorter sorter;

        private Integer pageNo;
        private Integer pageSize;


        public QueryCriteria build() {
            if (pageNo != null && pageSize != null) {
                var offset = offsetFromPage(pageNo, pageSize);
                return new QueryCriteria(projector, selector, sorter, offset, pageSize);
            }
            return new QueryCriteria(projector, selector, sorter, null, pageSize);
        }

        public Builder select(String script) throws ParseException {
            this.projector = Projector.parse(script);
            return this;
        }

        public Builder where(String script, Object value) throws ParseException {
            if (selector == null) {
                selector = new Selector();
            }
            selector.add(Selector.Predicate.parse(script, value));
            return this;
        }

        public Builder orderBy(String script) throws ParseException {
            this.sorter = Sorter.parse(script);
            return this;
        }

        public Builder page(String pageNo, String pageSize) {
            pageNo = StringUtils.trimToNull(pageNo);
            pageSize = StringUtils.trimToNull(pageSize);
            return page(pageNo == null ? null : Integer.parseInt(pageNo),
                    pageSize == null ? null : Integer.parseInt(pageSize));
        }

        public Builder page(Integer pageNo, Integer pageSize) {
            this.pageNo = pageNo;
            this.pageSize = pageSize;
            return this;
        }

    }


}
