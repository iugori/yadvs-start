package ro.iugori.yadvs.model.criteria;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SelectionPredicateTest {

    @Test
    void parse() throws ParseException {
        assertEquals("a=", SelectionFilter.Predicate.parse(" a =").toString());
    }

    @Test
    void parseError() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Predicate.parse("no equal"))
                .withMessage("Cannot parse selection predicate without name and value (`=' is missing).");
    }

    @Test
    void parse2() throws ParseException {
        assertEquals("a=b", SelectionFilter.Predicate.parse(" a ", "b").toString());
        assertEquals("a~gt=b", SelectionFilter.Predicate.parse(" a ~ > ", "b").toString());
    }

    @Test
    void parseError2() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Predicate.parse(null, ""))
                .withMessage("Cannot parse selection predicate without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Predicate.parse("abc", null))
                .withMessage("Cannot parse selection predicate with null value.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Predicate.parse("a]", "b"))
                .withMessage("Cannot parse selection predicate with field `a]' (must be a valid Java identifier).");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Predicate.parse("~gt", "b"))
                .withMessage("Cannot parse selection predicate with field `~gt' (must be a valid Java identifier).");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Predicate.parse("a~x", "b"))
                .withMessage("Cannot parse selection predicate operator `x'.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Predicate.parse("23~gt", "20"))
                .withMessage("Cannot parse selection predicate with field `23' (must be a valid Java identifier).");
    }

}