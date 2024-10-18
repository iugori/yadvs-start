package ro.iugori.yadvs.util;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConversionUtilTest {

    @Test
    void asCollection() {
        assertEquals(List.of(), ConversionUtil.asCollection(null, null));
        assertEquals(List.of(1, 2, 3), ConversionUtil.asCollection(Integer.class, "1,2,3"));
        assertEquals(List.of("1", "2", "3"), ConversionUtil.asCollection(String.class, new int[]{1, 2, 3}));
        assertEquals(List.of("1", "2", "3"), ConversionUtil.asCollection(String.class, List.of(1, 2, 3)));
        assertEquals(List.of(1L), ConversionUtil.asCollection(Long.class, 1));
    }

}