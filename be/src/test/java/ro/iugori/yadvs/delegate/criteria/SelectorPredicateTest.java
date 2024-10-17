package ro.iugori.yadvs.delegate.criteria;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SelectorPredicateTest {

    @Test
    void parse() throws ParseException {
        assertEquals("a=", Selector.Predicate.parse(" a =").toString());
    }

    @Test
    void parseError() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse("no equal"))
                .withMessage("Cannot parse selection predicate without name and value (`=' is missing).");
    }

    @Test
    void parse2() throws ParseException {
        assertEquals("a=b", Selector.Predicate.parse(" a ", "b").toString());
        assertEquals("a[gt]=b", Selector.Predicate.parse(" a [ > ] ", "b").toString());
    }

    @Test
    void parseError2() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse(null, ""))
                .withMessage("Cannot parse selection predicate without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse("abc", null))
                .withMessage("Cannot parse selection predicate with null value.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse("a]", "b"))
                .withMessage("Cannot parse selection predicate with field `a]' (must be a valid Java identifier).");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse("[gt]", "b"))
                .withMessage("Cannot parse selection predicate without name.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse("a[gt]x", "b"))
                .withMessage("Cannot parse selection predicate with invalid operation specifier.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse("a[x]", "b"))
                .withMessage("Cannot parse selection predicate operator `x'.");

        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Predicate.parse("23[gt]", "20"))
                .withMessage("Cannot parse selection predicate with field `23' (must be a valid Java identifier).");
    }

}