package ro.iugori.yadvs.delegate.criteria;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.junit.jupiter.api.Assertions.assertFalse;

class QueryCriteriaBuilderTest {

    @Test
    void builder() throws ParseException {
        var qc = QueryCriteria.builder()
                .select("a,b,c")
                .where("a~gt", "27")
                .orderBy("c,-d")
                .page(2, 10)
                .build();
        assertFalse(qc.isEmpty());
    }

}