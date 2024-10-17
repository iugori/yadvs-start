package ro.iugori.yadvs.delegate.refiner;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RecordRefinerTest {

    @Test
    void pageFromOffset() {
        assertEquals(1, RecordRefiner.pageFromOffset(0, 3));
        assertEquals(1, RecordRefiner.pageFromOffset(1, 3));
        assertEquals(1, RecordRefiner.pageFromOffset(2, 3));
        assertEquals(2, RecordRefiner.pageFromOffset(3, 3));
        assertEquals(2, RecordRefiner.pageFromOffset(4, 3));
        assertEquals(2, RecordRefiner.pageFromOffset(5, 3));
        assertEquals(3, RecordRefiner.pageFromOffset(6, 3));
    }

    @Test
    void offsetFromPage() {
        assertEquals(0, RecordRefiner.offsetFromPage(1, 1));
        assertEquals(0, RecordRefiner.offsetFromPage(1, 2));
        assertEquals(0, RecordRefiner.offsetFromPage(1, 3));
        assertEquals(1, RecordRefiner.offsetFromPage(2, 1));
        assertEquals(2, RecordRefiner.offsetFromPage(2, 2));
        assertEquals(3, RecordRefiner.offsetFromPage(2, 3));
        assertEquals(2, RecordRefiner.offsetFromPage(3, 1));
        assertEquals(4, RecordRefiner.offsetFromPage(3, 2));
        assertEquals(6, RecordRefiner.offsetFromPage(3, 3));
    }

    @Test
    void isEmpty() {
        assertTrue(new RecordRefiner(null, null, null, null, null).isEmpty());
        assertTrue(new RecordRefiner(new Projector(), new Selector(), new Sorter(), null, null).isEmpty());
        assertFalse(new RecordRefiner(null, null, null, 1, null).isEmpty());
        assertFalse(new RecordRefiner(null, null, null, null, 1).isEmpty());
    }

}