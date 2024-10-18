package ro.iugori.yadvs.repository;

import org.springframework.data.util.Pair;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.entity.PollEntity;

import java.util.List;

public interface PollRepositoryCustom {

    Pair<List<PollEntity>, Long> findByCriteriaAndCountTotal(CallContext callCtx, QueryCriteria qc);

    List<PollEntity> findByCriteria(CallContext callCtx, QueryCriteria qc);

}
