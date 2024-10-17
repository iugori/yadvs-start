package ro.iugori.yadvs.delegate.refiner;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SelectorOperatorTest {

    @Test
    void parse() throws ParseException {
        assertEquals(Selector.Operator.IN, Selector.Operator.parse("IN"));
        assertEquals(Selector.Operator.EQ, Selector.Operator.parse("=="));
        assertEquals(Selector.Operator.NE, Selector.Operator.parse("neq"));
    }

    @Test
    void parseError() {
        assertThatExceptionOfType(ParseException.class)
                .isThrownBy(() -> Selector.Operator.parse("*"))
                .withMessage("Cannot parse selection predicate operator `*'.");
    }

}