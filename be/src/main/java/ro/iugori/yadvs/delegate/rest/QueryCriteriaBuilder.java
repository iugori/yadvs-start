package ro.iugori.yadvs.delegate.rest;

import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.rest.RestContext;
import ro.iugori.yadvs.util.rest.RestApi;

public class QueryCriteriaBuilder {

    public static QueryCriteria of(RestContext restCtx) {
        var qcBuilder = QueryCriteria.builder();
        var queryParams = restCtx.getRequest().getParameterMap();

        String pageNoStr = null;
        String pageSizeStr = null;

        for (var entry : queryParams.entrySet()) {
            var key = entry.getKey();
            var valueArray = entry.getValue();
            if (valueArray == null || valueArray.length == 0) {
                continue;
            }
            var value = valueArray[0];
            switch (key) {
                case RestApi.Param.FIELDS -> qcBuilder.select(value);
                case RestApi.Param.SORT -> qcBuilder.orderBy(value);
                case RestApi.Param.PAGE_NO -> pageNoStr = value;
                case RestApi.Param.PAGE_SIZE -> pageSizeStr = value;
                default -> {
                    if (!key.startsWith(RestApi.RESERVED_PARAM)) {
                        qcBuilder.where(key, valueArray.length == 1 ? value : valueArray);
                    }
                }
            }
        }

        qcBuilder.page(pageNoStr, pageSizeStr);

        return qcBuilder.build();
    }

}
