package ro.iugori.yadvs.delegate.refiner;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ProjectorTest {

    @Test
    void parse() throws ParseException {
        assertEquals("", Projector.parse(null).toString());
        assertEquals("", Projector.parse("").toString());
        assertEquals("", Projector.parse(", ").toString());
        assertEquals("", Projector.parse(",,,").toString());
        assertEquals("a", Projector.parse("a").toString());
        assertEquals("a,b", Projector.parse("a, b").toString());
    }

    @Test
    void parseErrors() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Projector.parse("aw~2"))
                .withMessage("Cannot use projection field `aw~2' (must be a valid Java identifier).");
    }

}