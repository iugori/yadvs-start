package ro.iugori.yadvs.delegate.refiner;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SortFieldTest {

    @Test
    void parse() throws ParseException {
        assertEquals("+someName", SortField.parse("someName").toString());
        assertEquals("+someName", SortField.parse("  +  someName").toString());
        assertEquals("-someName", SortField.parse("-someName  ").toString());
    }

    @Test
    void parseErrors() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortField.parse(null))
                .withMessage("Cannot parse SortField from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortField.parse(""))
                .withMessage("Cannot parse SortField from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortField.parse("    "))
                .withMessage("Cannot parse SortField from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortField.parse("+"))
                .withMessage("Cannot parse SortField without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortField.parse(" - "))
                .withMessage("Cannot parse SortField without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortField.parse(" + - "))
                .withMessage("Cannot parse SortField with name `-' (must be a valid Java identifier).");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortField.parse("+ Lorem *&^"))
                .withMessage("Cannot parse SortField with name `Lorem *&^' (must be a valid Java identifier).");
    }


}