package ro.iugori.yadvs.model.criteria;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SortDirectionTest {

    @Test
    void parse() throws ParseException {
        assertEquals("", SortOrder.parse(null).toString());
        assertEquals("", SortOrder.parse("").toString());
        assertEquals("", SortOrder.parse(", ").toString());
        assertEquals("", SortOrder.parse(",,,").toString());
        assertEquals("+a", SortOrder.parse("a").toString());
        assertEquals("+a,-b", SortOrder.parse("a, - b").toString());
        assertEquals("+a,-b", SortOrder.parse("a, ,- b  ,").toString());
    }

}