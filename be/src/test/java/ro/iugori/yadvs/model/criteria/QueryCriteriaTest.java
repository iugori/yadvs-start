package ro.iugori.yadvs.model.criteria;

import org.junit.jupiter.api.Test;
import ro.iugori.yadvs.delegate.criteria.ProjectionFilter;
import ro.iugori.yadvs.delegate.criteria.QueryCriteria;
import ro.iugori.yadvs.delegate.criteria.SelectionFilter;
import ro.iugori.yadvs.delegate.criteria.SortOrder;

import static org.junit.jupiter.api.Assertions.*;

class QueryCriteriaTest {

    @Test
    void pageFromOffset() {
        assertEquals(1, QueryCriteria.pageFromOffset(0, 3));
        assertEquals(1, QueryCriteria.pageFromOffset(1, 3));
        assertEquals(1, QueryCriteria.pageFromOffset(2, 3));
        assertEquals(2, QueryCriteria.pageFromOffset(3, 3));
        assertEquals(2, QueryCriteria.pageFromOffset(4, 3));
        assertEquals(2, QueryCriteria.pageFromOffset(5, 3));
        assertEquals(3, QueryCriteria.pageFromOffset(6, 3));
    }

    @Test
    void offsetFromPage() {
        assertEquals(0, QueryCriteria.offsetFromPage(1, 1));
        assertEquals(0, QueryCriteria.offsetFromPage(1, 2));
        assertEquals(0, QueryCriteria.offsetFromPage(1, 3));
        assertEquals(1, QueryCriteria.offsetFromPage(2, 1));
        assertEquals(2, QueryCriteria.offsetFromPage(2, 2));
        assertEquals(3, QueryCriteria.offsetFromPage(2, 3));
        assertEquals(2, QueryCriteria.offsetFromPage(3, 1));
        assertEquals(4, QueryCriteria.offsetFromPage(3, 2));
        assertEquals(6, QueryCriteria.offsetFromPage(3, 3));
    }

    @Test
    void isEmpty() {
        assertTrue(new QueryCriteria(null, null, null, null, null).isEmpty());
        assertTrue(new QueryCriteria(new ProjectionFilter(), new SelectionFilter(), new SortOrder(), null, null).isEmpty());
        assertFalse(new QueryCriteria(null, null, null, 1, null).isEmpty());
        assertFalse(new QueryCriteria(null, null, null, null, 1).isEmpty());
    }

}
