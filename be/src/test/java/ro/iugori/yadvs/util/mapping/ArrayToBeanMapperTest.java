package ro.iugori.yadvs.util.mapping;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ArrayToBeanMapperTest {

    static class A {
        @Getter
        private String description;
    }

    @NoArgsConstructor
    static class B extends A {
        @Getter
        private String name;
    }

    @Test
    void of() {
        var mapper = ArrayToBeanMapper.of(B.class, List.of("name", "some", "description"));
        var bean = mapper.map(new Object[]{"Name", "Some", "Desc"});
        assertEquals("Name", bean.getName());
        assertEquals("Desc", bean.getDescription());
    }

}
