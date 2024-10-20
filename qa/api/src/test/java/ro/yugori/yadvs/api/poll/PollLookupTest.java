package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.RestRequests;

import java.util.stream.IntStream;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class PollLookupTest extends PollBaseTest {

    private static final int DUMMY_POLL_NO = 20;

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
        var polls = bodyAsJSONObject(rr).getJSONObject("_embedded").getJSONArray("pollList");
        assertThat(polls.length()).isGreaterThanOrEqualTo(DUMMY_POLL_NO);
    }

    @Test
    void getPage_1_5() {
        var rr = given().
                when()
                .param(RestRequests.Params.PAGE_NO, 1)
                .param(RestRequests.Params.PAGE_SIZE, 5)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_OK);
        var xTotalCount = rr.extract().header("X-Total-Count");
        assertThat(DUMMY_POLL_NO).isLessThanOrEqualTo(Integer.parseInt(xTotalCount));
        var polls = bodyAsJSONObject(rr).getJSONObject("_embedded").getJSONArray("pollList");
        assertThat(polls.length()).isGreaterThanOrEqualTo(5);
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
