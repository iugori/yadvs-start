package ro.iugori.yadvs.model.criteria;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ProjectionFilterTest {

    @Test
    void parse() throws ParseException {
        assertEquals("", ProjectionFilter.parse(null).toString());
        assertEquals("", ProjectionFilter.parse("").toString());
        assertEquals("", ProjectionFilter.parse(", ").toString());
        assertEquals("", ProjectionFilter.parse(",,,").toString());
        assertEquals("a", ProjectionFilter.parse("a").toString());
        assertEquals("a,b", ProjectionFilter.parse("a, b").toString());
    }

    @Test
    void parseErrors() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> ProjectionFilter.parse("aw~2"))
                .withMessage("Cannot use projection field `aw~2' (must be a valid Java identifier).");
    }

}