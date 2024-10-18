package ro.iugori.yadvs.repository;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.core.io.ResourceLoader;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.ctx.TestContext;
import ro.iugori.yadvs.model.entity.PollEntity;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@DataJpaTest
class PollRepositoryCustomTest {

    @Autowired
    private PollRepositoryCustom customRepository;

    private final TestContext testCtx = new TestContext();

    @BeforeAll
    public static void setUp(@Autowired PollRepository pollRepository) throws IOException {
        try (var resourceStream = ResourceLoader.class.getResourceAsStream("/data/poll-sample.json")) {
            if (resourceStream != null) {
                var content = new String(resourceStream.readAllBytes(), StandardCharsets.UTF_8);
                var objectMapper = new ObjectMapper();
                objectMapper.registerModule(new JavaTimeModule());
                var dtoList = objectMapper.readValue(content, new TypeReference<List<PollEntity>>() {
                });
                dtoList.forEach(poll -> {
                    poll.setId(null);
                    pollRepository.saveAndFlush(poll);
                });
            }
        }
    }

    @Test
    void whereEQ() {
        var name = "Future Usability Representative";
        var qc = QueryCriteria.builder().where("name", name).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(name, selection.get(0).getName());
    }

    @Test
    void whereNE() {
        var name = "District Program Specialist";
        var qc = QueryCriteria.builder().where("name[ne]", name).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(29, selection.size());
    }

    @Test
    void whereISTrue() {
        var qc = QueryCriteria.builder().where("multiOption[is]", true).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(14, selection.size());
        assertTrue(selection.get(0).isMultiOption());
    }

    @Test
    void whereISFalse() {
        var qc = QueryCriteria.builder().where("multiOption[is]", false).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(16, selection.size());
    }

    @Test
    void whereISNull() {
        var qc = QueryCriteria.builder()
                .where("start[is]", "null")
                .where("end[is]", "null")
                .build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(1, selection.size());
        assertNull(selection.get(0).getStart());
        assertNull(selection.get(0).getEnd());
    }

    @Test
    void whereISNotNull() {
        var qc = QueryCriteria.builder()
                .where("start[is]", "null")
                .where("end[is]", "notnull")
                .build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(1, selection.size());
        assertNull(selection.get(0).getStart());
        assertNotNull(selection.get(0).getEnd());
    }

    @Test
    void whereLTNumeric() {
        var qc = QueryCriteria.builder().where("id[lt]", 4).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(3, selection.size());
    }

    @Test
    void whereLTNonNumeric() {
        var qc = QueryCriteria.builder().where("start[lt]", "2024-10-10T14:54:50.769").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(9, selection.size());
    }

    @Test
    void whereLENumeric() {
        var qc = QueryCriteria.builder().where("id[le]", 4).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(4, selection.size());
    }

    @Test
    void whereLENonNumeric() {
        var qc = QueryCriteria.builder().where("start[le]", "2024-10-10T14:54:50.769").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(10, selection.size());
    }

    @Test
    void whereGTNumeric() {
        var qc = QueryCriteria.builder().where("id[gt]", 4).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(26, selection.size());
    }

    @Test
    void whereLGTNonNumeric() {
        var qc = QueryCriteria.builder().where("start[gt]", "2024-10-10T14:54:50.769").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(18, selection.size());
    }

    @Test
    void whereGENumeric() {
        var qc = QueryCriteria.builder().where("id[ge]", 4).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(27, selection.size());
    }

    @Test
    void whereGENonNumeric() {
        var qc = QueryCriteria.builder().where("start[ge]", "2024-10-10T14:54:50.769").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(19, selection.size());
    }

    @Test
    void whereLike() {
        var qc = QueryCriteria.builder().where("description[like]", "non").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(7, selection.size());
    }

    @Test
    void whereUnlike() {
        var qc = QueryCriteria.builder().where("description[unlike]", "non").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(23, selection.size());
    }

    @Test
    void whereINNumeric() {
        var qc = QueryCriteria.builder().where("id[in]", "11,12,13").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(3, selection.size());
    }

    @Test
    void whereINText() {
        var qc = QueryCriteria.builder().where("name[in]", List.of("Manager", "Dreamer", "Consultant")).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(2, selection.size());
    }

    @Test
    void orderByNameAsc() {
        var qc = QueryCriteria.builder().orderBy("+name").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals("Chief Research Associate", selection.get(0).getName());
    }

    @Test
    void orderByDescriptionDesc() {
        var qc = QueryCriteria.builder().orderBy("-description").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals("Product Division Orchestrator", selection.get(0).getName());
    }

    @Test
    void orderByMultiOptionDescNameAsc() {
        var qc = QueryCriteria.builder().orderBy("-multiOption,+name").build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals("Consultant", selection.get(0).getName());
    }

    @Test
    void page_1_4() {
        var qc = QueryCriteria.builder().orderBy("id").page(1, 4).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(1, selection.get(0).getId());
        assertEquals(4, selection.size());
    }

    @Test
    void page_2_4() {
        var qc = QueryCriteria.builder().orderBy("id").page(2, 4).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(5, selection.get(0).getId());
        assertEquals(4, selection.size());
    }

    @Test
    void page_2_20() {
        var qc = QueryCriteria.builder().orderBy("id").page(2, 20).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(10, selection.size());
    }

    @Test
    void page_3_20() {
        var qc = QueryCriteria.builder().orderBy("id").page(3, 20).build();
        var selection = customRepository.findByCriteria(testCtx, qc);
        assertEquals(0, selection.size());
    }

}