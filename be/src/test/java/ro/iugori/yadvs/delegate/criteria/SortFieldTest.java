package ro.iugori.yadvs.delegate.criteria;

import org.junit.jupiter.api.Test;
import ro.iugori.yadvs.delegate.criteria.SortOrder;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SortFieldTest {

    @Test
    void parse() throws ParseException {
        assertEquals("+someName", SortOrder.Field.parse("someName").toString());
        assertEquals("+someName", SortOrder.Field.parse("  +  someName").toString());
        assertEquals("-someName", SortOrder.Field.parse("-someName  ").toString());
    }

    @Test
    void parseErrors() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortOrder.Field.parse(null))
                .withMessage("Cannot parse sort field from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortOrder.Field.parse(""))
                .withMessage("Cannot parse sort field from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortOrder.Field.parse("    "))
                .withMessage("Cannot parse sort field from empty string.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortOrder.Field.parse("+"))
                .withMessage("Cannot parse sort field without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortOrder.Field.parse(" - "))
                .withMessage("Cannot parse sort field without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortOrder.Field.parse(" + - "))
                .withMessage("Cannot parse sort field `-' (must be a valid Java identifier).");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SortOrder.Field.parse("+ Lorem *&^"))
                .withMessage("Cannot parse sort field `Lorem *&^' (must be a valid Java identifier).");
    }

}