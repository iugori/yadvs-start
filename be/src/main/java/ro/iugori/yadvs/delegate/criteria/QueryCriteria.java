package ro.iugori.yadvs.delegate.criteria;

import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.model.domain.TargetType;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.model.rest.RestRequests;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

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

        private final List<ErrorModel> errors = new ArrayList<>();

        public QueryCriteria build() {
            if (!errors.isEmpty()) {
                throw new CheckException(errors);
            }
            if (pageNo != null && pageSize != null) {
                var offset = offsetFromPage(pageNo, pageSize);
                return new QueryCriteria(projector, selector, sorter, offset, pageSize);
            }
            return new QueryCriteria(projector, selector, sorter, null, pageSize);
        }

        public Builder select(String script) {
            try {
                this.projector = Projector.parse(script);
            } catch (ParseException e) {
                var error = new ErrorModel();
                error.setCode(ErrorCode.PROJECTION_CRITERIA);
                error.setMessage(e.getMessage());
                error.setTarget(TargetType.PARAMETER, RestRequests.Params.FIELDS);
                errors.add(error);
            }
            return this;
        }

        public Builder where(String script, Object value) {
            if (selector == null) {
                selector = new Selector();
            }
            try {
                selector.add(Selector.Predicate.parse(script, value));
            } catch (ParseException e) {
                var error = new ErrorModel();
                error.setCode(ErrorCode.SELECTION_CRITERIA);
                error.setMessage(e.getMessage());
                error.setTarget(TargetType.PARAMETER, script);
                errors.add(error);
            }
            return this;
        }

        public Builder orderBy(String script) {
            try {
                this.sorter = Sorter.parse(script);
            } catch (ParseException e) {
                var error = new ErrorModel();
                error.setCode(ErrorCode.SORTING_CRITERIA);
                error.setMessage(e.getMessage());
                error.setTarget(TargetType.PARAMETER, RestRequests.Params.SORT);
                errors.add(error);
            }
            return this;
        }

        public Builder page(String pageNoStr, String pageSizeStr) {
            Integer pageNo = null;
            pageNoStr = StringUtils.trimToNull(pageNoStr);
            if (pageNoStr != null) {
                try {
                    pageNo = Integer.parseInt(pageNoStr);
                } catch (NumberFormatException e) {
                    var error = new ErrorModel();
                    error.setCode(ErrorCode.PAGINATION_CRITERIA);
                    error.setMessage(e.getMessage());
                    error.setTarget(TargetType.PARAMETER, RestRequests.Params.PAGE_NO);
                    errors.add(error);
                }
            }

            Integer pageSize = null;
            pageSizeStr = StringUtils.trimToNull(pageSizeStr);
            if (pageSizeStr != null) {
                try {
                    pageSize = Integer.parseInt(pageSizeStr);
                } catch (NumberFormatException e) {
                    var error = new ErrorModel();
                    error.setCode(ErrorCode.PAGINATION_CRITERIA);
                    error.setMessage(e.getMessage());
                    error.setTarget(TargetType.PARAMETER, RestRequests.Params.PAGE_SIZE);
                    errors.add(error);
                }
            }

            return page(pageNo, pageSize);
        }

        public Builder page(Integer pageNo, Integer pageSize) {
            this.pageNo = pageNo;
            this.pageSize = pageSize;
            return this;
        }

    }

}
