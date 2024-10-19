package ro.iugori.yadvs.util.validation;

import jakarta.validation.Validation;
import jakarta.validation.Validator;
import org.junit.jupiter.api.Test;
import ro.iugori.yadvs.model.rest.Poll;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDateTime;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SuppressWarnings("unused")
class XFieldComparisonTest {

    private static final Validator validator;

    static {
        try (var validationFactory = Validation.buildDefaultValidatorFactory()) {
            validator = validationFactory.getValidator();
        }
    }

    @Test
    void isValid() {
        var poll = Poll.builder().name("Nemo").description("captain").build();
        var violations = validator.validate(poll);
        assertTrue(violations.isEmpty());

        poll.setEnd(LocalDateTime.now());
        violations = validator.validate(poll);
        assertTrue(violations.isEmpty());

        poll.setStart(poll.getEnd());
        violations = validator.validate(poll);
        assertFalse(violations.isEmpty());

        poll.setEnd(null);
        violations = validator.validate(poll);
        assertTrue(violations.isEmpty());

        poll.setEnd(LocalDateTime.now().plusHours(1L));
        violations = validator.validate(poll);
        assertTrue(violations.isEmpty());
    }

    @Test
    void testEq() {
        @XFieldComparison(field1 = "v1", rel = "eq", field2 = "v2")
        class Dto {
            final int v1 = 0;
            final double v2 = 0.;
        }
        var dto = new Dto();
        var violations = validator.validate(dto);
        assertTrue(violations.isEmpty());
    }

    @Test
    void testNe() {
        @XFieldComparison(field1 = "v1", rel = "ne", field2 = "v2")
        class Dto {
            final double v1 = 0.;
            final Long v2 = 0L;
        }
        var dto = new Dto();
        var violations = validator.validate(dto);
        assertFalse(violations.isEmpty());
    }

    @Test
    void testLt() {
        @XFieldComparison(field1 = "v1", rel = "lt", field2 = "v2")
        class Dto {
            BigInteger v1 = new BigInteger("12345678901234567890123456789012345678912345678890");
            BigDecimal v2 = new BigDecimal("12345678901234567890123456789012345678912345678891");
        }
        var dto = new Dto();
        var violations = validator.validate(dto);
        assertTrue(violations.isEmpty());

        dto.v1 = new BigInteger("12345678901234567890123456789012345678912345678890");
        dto.v2 = new BigDecimal("12345678901234567890123456789012345678912345678890");
        violations = validator.validate(dto);
        assertFalse(violations.isEmpty());
    }

    @Test
    void testLe() {
        @XFieldComparison(field1 = "v1", rel = "le", field2 = "v2")
        class Dto {
            java.sql.Date v1 = new java.sql.Date(1L);
            java.util.Date v2 = new java.util.Date(1L);
        }
        var dto = new Dto();
        var violations = validator.validate(dto);
        assertTrue(violations.isEmpty());

        dto.v1 = new java.sql.Date(1L);
        dto.v2 = new java.util.Date(2L);
        violations = validator.validate(dto);
        assertTrue(violations.isEmpty());
    }

    @Test
    void testGe() {
        @XFieldComparison(field1 = "v1", rel = "ge", field2 = "v2")
        class Dto {
            String v1 = "abc";
            String v2 = "abc";
        }
        var dto = new Dto();
        var violations = validator.validate(dto);
        assertTrue(violations.isEmpty());

        dto.v1 = "abcd";
        dto.v2 = "abc";
        violations = validator.validate(dto);
        assertTrue(violations.isEmpty());
    }

    @Test
    void testGt() {
        @XFieldComparison(field1 = "v1", rel = "gt", field2 = "v2")
        class Dto {
            final boolean v1 = true;
            final Boolean v2 = false;
        }
        var dto = new Dto();
        var violations = validator.validate(dto);
        assertTrue(violations.isEmpty());
    }

    @Test
    void testIncompatibleTypes() {
        @XFieldComparison(field1 = "v1", rel = "gt", field2 = "v2")
        class Dto {
            final String v1 = "true";
            final Boolean v2 = false;
        }
        var dto = new Dto();
        assertThatExceptionOfType(jakarta.validation.ValidationException.class)
                .isThrownBy(() -> validator.validate(dto))
                .withCauseInstanceOf(IllegalArgumentException.class)
                .extracting(Throwable::getCause)
                .extracting(Throwable::getMessage)
                .isEqualTo("Comparison is undefined for types java.lang.String and java.lang.Boolean.");
    }

}