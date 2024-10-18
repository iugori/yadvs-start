package ro.iugori.yadvs.model.criteria;

import ro.iugori.yadvs.delegate.criteria.QueryCriteriaBuilder;

public record QueryCriteria(
        ProjectionFilter projectionFilter,
        SelectionFilter selectionFilter,
        SortOrder sortOrder,
        Integer offset, Integer limit) {

    public static QueryCriteriaBuilder builder() {
        return new QueryCriteriaBuilder();
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
        return !isProjection() && !isSelection() && !isSort() && offset == null && limit == null;
    }

    public boolean isProjection() {
        return projectionFilter != null && !projectionFilter.isEmpty();
    }

    public boolean isSelection() {
        return selectionFilter != null && !selectionFilter.isEmpty();
    }

    public boolean isSort() {
        return sortOrder != null && !sortOrder.isEmpty();
    }

}
