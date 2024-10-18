package ro.iugori.yadvs.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TextUtilTest {

    @Test
    void isValidIdentifier() {
        assertTrue(TextUtil.isValidIdentifier("abc123"));
    }

    @Test
    void isInValidIdentifier() {
        assertFalse(TextUtil.isValidIdentifier("*"));
    }

}