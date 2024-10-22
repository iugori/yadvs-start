package ro.iugori.yadvs.delegate.query;

import jakarta.persistence.criteria.*;
import ro.iugori.yadvs.model.criteria.ProjectionFilter;
import ro.iugori.yadvs.model.criteria.SelectionFilter;
import ro.iugori.yadvs.model.criteria.SortOrder;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.util.conversion.ConversionUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

public class CriteriaBuilderDelegate {

    private final CallContext callCtx;
    private final CriteriaBuilder cb;
    private final CriteriaQuery<?> query;
    private final Root<?> root;

    public CriteriaBuilderDelegate(CallContext callCtx, CriteriaBuilder cb, CriteriaQuery<?> query, Root<?> root) {
        this.callCtx = callCtx;
        this.cb = cb;
        this.query = query;
        this.root = root;
    }

    public List<Selection<?>> buildProjection(ProjectionFilter columns) {
        var projection = new ArrayList<Selection<?>>();
        for (var column : columns) {
            projection.add(root.get(column));
        }
        return projection;
    }

    public List<Predicate> addWhereClauses(SelectionFilter filters) {
        var predicates = new ArrayList<Predicate>();
        if (filters != null) {
            for (var filter : filters) {
                try {
                    var path = root.get(filter.getName());
                    var predicate = buildPredicate(path, filter);
                    predicate.ifPresent(predicates::add);
                } catch (IllegalStateException | IllegalArgumentException ex) {
                    callCtx.getLogger().warn("{} {}", callCtx.getLogRef(), ex.getMessage());
                }
            }
            if (!predicates.isEmpty()) {
                query.where(cb.and(predicates.toArray(new Predicate[0])));
            }
        }
        return predicates;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private Optional<Predicate> buildPredicate(Path<?> path, SelectionFilter.Predicate filter) {
        switch (filter.getOp()) {
            case EQ -> {
                return Optional.of(cb.equal(path, filter.getValue()));
            }
            case NE -> {
                return Optional.of(cb.notEqual(path, filter.getValue()));
            }
            case IS -> {
                return buildIsPredicate(path, filter);
            }
            case LT -> {
                var targetType = path.getJavaType();
                if (Number.class.isAssignableFrom(targetType)) {
                    return Optional.of(cb.lt((Path<Number>) path, ConversionUtil.as(Number.class, filter.getValue())));
                }
                return Optional.of(cb.lessThan((Path<Comparable>) path, (Comparable) ConversionUtil.as(targetType, filter.getValue())));
            }
            case LE -> {
                var targetType = path.getJavaType();
                if (Number.class.isAssignableFrom(targetType)) {
                    return Optional.of(cb.le((Path<Number>) path, ConversionUtil.as(Number.class, filter.getValue())));
                }
                return Optional.of(cb.lessThanOrEqualTo((Path<Comparable>) path, (Comparable) ConversionUtil.as(targetType, filter.getValue())));
            }
            case GE -> {
                var targetType = path.getJavaType();
                if (Number.class.isAssignableFrom(targetType)) {
                    return Optional.of(cb.ge((Path<Number>) path, ConversionUtil.as(Number.class, filter.getValue())));
                }
                return Optional.of(cb.greaterThanOrEqualTo((Path<Comparable>) path, (Comparable) ConversionUtil.as(targetType, filter.getValue())));
            }
            case GT -> {
                var targetType = path.getJavaType();
                if (Number.class.isAssignableFrom(targetType)) {
                    return Optional.of(cb.gt((Path<Number>) path, ConversionUtil.as(Number.class, filter.getValue())));
                }
                return Optional.of(cb.greaterThan((Path<Comparable>) path, (Comparable) ConversionUtil.as(targetType, filter.getValue())));
            }
            case LIKE -> {
                if (String.class.equals(path.getJavaType())) {
                    return Optional.of(cb.like((Path<String>) path, String.format("%%%s%%", filter.getValue())));
                }
            }
            case UNLIKE -> {
                if (String.class.equals(path.getJavaType())) {
                    return Optional.of(cb.notLike((Path<String>) path, String.format("%%%s%%", filter.getValue())));
                }
            }
            case IN -> {
                return Optional.of(path.in(ConversionUtil.asCollection(path.getJavaType(), filter.getValue())));
            }
        }
        return Optional.empty();
    }


    @SuppressWarnings("unchecked")
    private Optional<Predicate> buildIsPredicate(Path<?> path, SelectionFilter.Predicate filter) {
        var value = String.valueOf(filter.getValue()).trim().toLowerCase(Locale.ROOT);
        switch (value) {
            case "null" -> {
                return Optional.of(cb.isNull(path));
            }
            case "notnull" -> {
                return Optional.of(cb.isNotNull(path));
            }
            case "true" -> {
                if (Boolean.class.equals(path.getJavaType())) {
                    return Optional.of(cb.isTrue((Path<Boolean>) path));
                }
            }
            case "false" -> {
                if (Boolean.class.equals(path.getJavaType())) {
                    return Optional.of(cb.isFalse((Path<Boolean>) path));
                }
            }
        }
        return Optional.empty();
    }

    public void addOrderBy(SortOrder byFields) {
        var columns = new ArrayList<Order>();
        for (var field : byFields) {
            var column = root.get(field.getName());
            var order = field.getDirection() == SortOrder.Direction.DESC ? cb.desc(column) : cb.asc(column);
            columns.add(order);
        }
        if (!columns.isEmpty()) {
            query.orderBy(columns);
        }
    }


}
