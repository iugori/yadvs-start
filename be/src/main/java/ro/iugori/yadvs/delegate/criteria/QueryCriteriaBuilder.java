package ro.iugori.yadvs.delegate.criteria;

import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.util.rest.RestApi;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

public class QueryCriteriaBuilder {

    private ProjectionFilter projectionFilter;
    private SelectionFilter selectionFilter;
    private SortOrder sortOrder;
    private Integer pageNo;
    private Integer pageSize;

    private final List<ErrorModel> errors = new ArrayList<>();

    public QueryCriteria build() {
        if (!errors.isEmpty()) {
            throw new CheckException(errors);
        }
        if (pageNo != null && pageSize != null) {
            var offset = QueryCriteria.offsetFromPage(pageNo, pageSize);
            return new QueryCriteria(projectionFilter, selectionFilter, sortOrder, offset, pageSize);
        }
        return new QueryCriteria(projectionFilter, selectionFilter, sortOrder, null, pageSize);
    }

    public QueryCriteriaBuilder select(String script) {
        try {
            this.projectionFilter = ProjectionFilter.parse(script);
        } catch (ParseException e) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.PROJECTION_CRITERIA);
            error.setMessage(e.getMessage());
            error.setTarget(TargetType.PARAMETER, RestApi.Param.FIELDS);
            errors.add(error);
        }
        return this;
    }

    public QueryCriteriaBuilder where(String script) {
        if (selectionFilter == null) {
            selectionFilter = new SelectionFilter();
        }
        try {
            selectionFilter.add(SelectionFilter.Predicate.parse(script));
        } catch (ParseException e) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.SELECTION_CRITERIA);
            error.setMessage(e.getMessage());
            error.setTarget(TargetType.PARAMETER, script);
            errors.add(error);
        }
        return this;
    }

    public QueryCriteriaBuilder where(String script, Object value) {
        if (selectionFilter == null) {
            selectionFilter = new SelectionFilter();
        }
        try {
            selectionFilter.add(SelectionFilter.Predicate.parse(script, value));
        } catch (ParseException e) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.SELECTION_CRITERIA);
            error.setMessage(e.getMessage());
            error.setTarget(TargetType.PARAMETER, script);
            errors.add(error);
        }
        return this;
    }

    public QueryCriteriaBuilder orderBy(String script) {
        try {
            this.sortOrder = SortOrder.parse(script);
        } catch (ParseException e) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.SORTING_CRITERIA);
            error.setMessage(e.getMessage());
            error.setTarget(TargetType.PARAMETER, RestApi.Param.SORT);
            errors.add(error);
        }
        return this;
    }

    public QueryCriteriaBuilder page(String pageNoStr, String pageSizeStr) {
        Integer pageNo = null;
        pageNoStr = StringUtils.trimToNull(pageNoStr);
        if (pageNoStr != null) {
            try {
                pageNo = Integer.parseInt(pageNoStr);
            } catch (NumberFormatException e) {
                var error = new ErrorModel();
                error.setCode(ErrorCode.PAGINATION_CRITERIA);
                error.setMessage(e.getMessage());
                error.setTarget(TargetType.PARAMETER, RestApi.Param.PAGE_NO);
                errors.add(error);
            }
        }
        if (pageNo != null && pageNo < 1) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.PAGINATION_CRITERIA);
            error.setMessage("Must be greater than or equal to 1.");
            error.setTarget(TargetType.PARAMETER, RestApi.Param.PAGE_NO);
            errors.add(error);
        }

        Integer pageSize = null;
        pageSizeStr = StringUtils.trimToNull(pageSizeStr);
        if (pageSizeStr != null) {
            try {
                pageSize = Integer.parseInt(pageSizeStr);
                if (pageNo == null) {
                    pageNo = 1;
                }
            } catch (NumberFormatException e) {
                var error = new ErrorModel();
                error.setCode(ErrorCode.PAGINATION_CRITERIA);
                error.setMessage(e.getMessage());
                error.setTarget(TargetType.PARAMETER, RestApi.Param.PAGE_SIZE);
                errors.add(error);
            }
        }

        if (pageSize != null && pageSize < 1) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.PAGINATION_CRITERIA);
            error.setMessage("Must be greater than or equal to 1.");
            error.setTarget(TargetType.PARAMETER, RestApi.Param.PAGE_SIZE);
            errors.add(error);
        }

        if (pageNo != null && pageSize == null) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.PAGINATION_CRITERIA);
            error.setMessage(String.format("Cannot have `%s' without `%s'.",
                    RestApi.Param.PAGE_NO, RestApi.Param.PAGE_SIZE));
            error.setTarget(TargetType.PARAMETER, RestApi.Param.PAGE_NO);
            errors.add(error);
        }

        return page(pageNo, pageSize);
    }

    public QueryCriteriaBuilder page(Integer pageNo, Integer pageSize) {
        this.pageNo = pageNo;
        this.pageSize = pageSize;
        return this;
    }

}
