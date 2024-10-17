package ro.iugori.yadvs.delegate.refiner;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SorterTest {

    @Test
    void parse() throws ParseException {
        assertEquals("", Sorter.parse(null).toString());
        assertEquals("", Sorter.parse("").toString());
        assertEquals("", Sorter.parse(", ").toString());
        assertEquals("", Sorter.parse(",,,").toString());
        assertEquals("+a", Sorter.parse("a").toString());
        assertEquals("+a,-b", Sorter.parse("a, - b").toString());
        assertEquals("+a,-b", Sorter.parse("a, ,- b  ,").toString());
    }

}