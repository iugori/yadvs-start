package ro.iugori.yadvs.repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaQuery;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Repository;
import ro.iugori.yadvs.delegate.query.CriteriaBuilderDelegate;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.entity.PollEntity;
import ro.iugori.yadvs.util.mapping.ArrayToBeanMapper;

import java.util.List;
import java.util.stream.Collectors;

@Repository
public class PollRepositoryImpl implements PollRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public Pair<List<PollEntity>, Long> findByCriteriaAndCountTotal(CallContext callCtx, QueryCriteria qc) {
        return findByCriteriaAndCountTotal(callCtx, qc, true);
    }

    @Override
    public List<PollEntity> findByCriteria(CallContext callCtx, QueryCriteria qc) {
        return findByCriteriaAndCountTotal(callCtx, qc, false).getFirst();
    }

    @SuppressWarnings("unchecked")
    private Pair<List<PollEntity>, Long> findByCriteriaAndCountTotal(CallContext callCtx, QueryCriteria qc, boolean countTotal) {
        var cb = entityManager.getCriteriaBuilder();
        var query = qc.isProjection() ? cb.createQuery() : cb.createQuery(PollEntity.class);
        var entity = query.from(PollEntity.class);

        var cbDelegate = new CriteriaBuilderDelegate(callCtx, cb, query, entity);

        // projection
        if (qc.isProjection()) {
            query.multiselect(cbDelegate.buildProjection(qc.projectionFilter()));
        } else {
            ((CriteriaQuery<PollEntity>) query).select(entity);
        }

        // selection
        var predicates = cbDelegate.addWhereClauses(qc.selectionFilter());

        // sorting
        if (qc.isSort()) {
            cbDelegate.addOrderBy(qc.sortOrder());
        }

        // pagination
        var typedQuery = entityManager.createQuery(query);
        if (qc.offset() != null && qc.offset() > 0) {
            typedQuery.setFirstResult(qc.offset());
        }
        if (qc.limit() != null && qc.limit() > 0) {
            typedQuery.setMaxResults(qc.limit());
        }

        Long totalCount = null;
        // counting total - no need to run the counter query if pagination is not applied
        if (countTotal && qc.offset() != null && qc.limit() != null) {
            var countQuery = cb.createQuery(Long.class);
            var countEntity = countQuery.from(PollEntity.class);
            var countDelegate = new CriteriaBuilderDelegate(callCtx, cb, countQuery, countEntity);
            countQuery.select(cb.count(countEntity));
            countDelegate.addWhereClauses(qc.selectionFilter());
            totalCount = entityManager.createQuery(countQuery).getSingleResult();
        }

        var result = typedQuery.getResultList();
        if (qc.isProjection()) {
            var mapper = ArrayToBeanMapper.of(PollEntity.class, qc.projectionFilter());
            result = result.stream().map(a -> mapper.map((Object[]) a)).collect(Collectors.toList());
        }
        if (totalCount == null) {
            totalCount = (long) result.size();
        }
        return Pair.of((List<PollEntity>) result, totalCount);
    }

}
