package ro.iugori.yadvs.delegate.criteria;

import org.junit.jupiter.api.Test;
import ro.iugori.yadvs.delegate.criteria.SelectionFilter;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SelectionOperatorTest {

    @Test
    void parse() throws ParseException {
        assertEquals(SelectionFilter.Operator.EQ, SelectionFilter.Operator.parse("EQ"));
        assertEquals(SelectionFilter.Operator.EQ, SelectionFilter.Operator.parse("=="));

        assertEquals(SelectionFilter.Operator.IS, SelectionFilter.Operator.parse("IS"));

        assertEquals(SelectionFilter.Operator.LT, SelectionFilter.Operator.parse("LT"));
        assertEquals(SelectionFilter.Operator.LT, SelectionFilter.Operator.parse("<"));

        assertEquals(SelectionFilter.Operator.LE, SelectionFilter.Operator.parse("LE"));
        assertEquals(SelectionFilter.Operator.LE, SelectionFilter.Operator.parse("<="));
        assertEquals(SelectionFilter.Operator.LE, SelectionFilter.Operator.parse("LTE"));

        assertEquals(SelectionFilter.Operator.NE, SelectionFilter.Operator.parse("NE"));
        assertEquals(SelectionFilter.Operator.NE, SelectionFilter.Operator.parse("!="));
        assertEquals(SelectionFilter.Operator.NE, SelectionFilter.Operator.parse("NEQ"));

        assertEquals(SelectionFilter.Operator.GE, SelectionFilter.Operator.parse("GE"));
        assertEquals(SelectionFilter.Operator.GE, SelectionFilter.Operator.parse(">="));
        assertEquals(SelectionFilter.Operator.GE, SelectionFilter.Operator.parse("GTE"));

        assertEquals(SelectionFilter.Operator.IN, SelectionFilter.Operator.parse("IN"));

        assertEquals(SelectionFilter.Operator.LIKE, SelectionFilter.Operator.parse("LIKE"));

        assertEquals(SelectionFilter.Operator.UNLIKE, SelectionFilter.Operator.parse("UNLIKE"));
    }

    @Test
    void parseError() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> SelectionFilter.Operator.parse("*"))
                .withMessage("Cannot parse selection predicate operator `*'.");
    }

}