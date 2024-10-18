package ro.iugori.yadvs.repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.springframework.stereotype.Repository;
import ro.iugori.yadvs.delegate.criteria.CriteriaBuilderDelegate;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.entity.PollEntity;

import java.util.List;

@Repository
public class PollRepositoryImpl implements PollRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<PollEntity> findByCriteria(CallContext callCtx, QueryCriteria qc) {
        var cb = entityManager.getCriteriaBuilder();
        var query = cb.createQuery(PollEntity.class);
        var pollEntity = query.from(PollEntity.class);

        var qcDelegate = new CriteriaBuilderDelegate(callCtx, cb, query, pollEntity);

        query.select(pollEntity);
        qcDelegate.addWhereConjunction(qc.selectionFilter());
        qcDelegate.addOrderBy(qc.sortCriteria());

        var typedQuery = entityManager.createQuery(query);
        if (qc.offset() != null && qc.offset() > 0) {
            typedQuery.setFirstResult(qc.offset());
        }
        if (qc.limit() != null && qc.limit() > 0) {
            typedQuery.setMaxResults(qc.limit());
        }

        return typedQuery.getResultList();
    }

}
