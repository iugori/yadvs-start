package ro.iugori.yadvs.repository;

import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.entity.PollEntity;

import java.util.List;

public interface PollRepositoryCustom {

    List<PollEntity> findByCriteria(CallContext callCtx, QueryCriteria qc);

}
