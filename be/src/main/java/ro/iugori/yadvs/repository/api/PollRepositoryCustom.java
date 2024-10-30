package ro.iugori.yadvs.repository.api;

import org.springframework.data.util.Pair;
import ro.iugori.yadvs.delegate.criteria.QueryCriteria;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.entity.PollEntity;

import java.util.List;

public interface PollRepositoryCustom {

    // TODO: investigate the opportunity to use entity graphs
    Pair<List<PollEntity>, Long> findByCriteriaAndCountTotal(CallContext callCtx, QueryCriteria qc);

    List<PollEntity> findByCriteria(CallContext callCtx, QueryCriteria qc);

}
