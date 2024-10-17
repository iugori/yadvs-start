package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.stream.IntStream;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class PollLookupTest extends PollBaseTest {

    private static final int DUMMY_POLL_NO = 30;

    @BeforeAll
    static void addSomePolls() {
        IntStream.range(0, DUMMY_POLL_NO).forEach(i -> createPoll(nextPoll()));
    }

    @Test
    void getAll() {
        var rr = given().
                when()
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_OK);
        var bodyJson = bodyAsJSONArray(rr);
        assertThat(bodyJson.length()).isGreaterThanOrEqualTo(DUMMY_POLL_NO);
    }

    @Test
    void getNone() {
        given().
                when()
                .param("name", "")
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT);
    }

}
