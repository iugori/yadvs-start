package ro.iugori.yadvs.delegate.refiner;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SortFieldTest {

    @Test
    void parse() throws ParseException {
        assertEquals("+someName", Sorter.Field.parse("someName").toString());
        assertEquals("+someName", Sorter.Field.parse("  +  someName").toString());
        assertEquals("-someName", Sorter.Field.parse("-someName  ").toString());
    }

    @Test
    void parseErrors() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Sorter.Field.parse(null))
                .withMessage("Cannot parse sort field from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Sorter.Field.parse(""))
                .withMessage("Cannot parse sort field from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Sorter.Field.parse("    "))
                .withMessage("Cannot parse sort field from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Sorter.Field.parse("+"))
                .withMessage("Cannot parse sort field without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Sorter.Field.parse(" - "))
                .withMessage("Cannot parse sort field without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Sorter.Field.parse(" + - "))
                .withMessage("Cannot parse sort field `-' (must be a valid Java identifier).");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Sorter.Field.parse("+ Lorem *&^"))
                .withMessage("Cannot parse sort field `Lorem *&^' (must be a valid Java identifier).");
    }


}