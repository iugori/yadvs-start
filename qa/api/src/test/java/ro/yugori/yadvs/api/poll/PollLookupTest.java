package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.RestApi;

import java.util.stream.IntStream;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.hamcrest.Matchers.notNullValue;

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
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        var polls = bodyAsJSONObject(rr).getJSONObject("_embedded").getJSONArray("pollList");
        assertThat(polls.length()).isGreaterThanOrEqualTo(DUMMY_POLL_NO);
    }

    @Test
    void getProjection1() {
        var rr = given().
                when()
                .param(RestApi.Param.FIELDS, "id")
                .param(RestApi.Param.PAGE_SIZE, 1)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        var polls = bodyAsJSONObject(rr).getJSONObject("_embedded").getJSONArray("pollList");
        assertThat(polls.length()).isEqualTo(1);
        var poll = polls.getJSONObject(0);
        assertThat(poll.has("id")).isTrue();
        assertThat(poll.has("name")).isFalse();
        assertThat(poll.has("description")).isFalse();
        assertThat(poll.has("status")).isFalse();
        assertThat(poll.has("multiOption")).isFalse();
        assertThat(poll.has("start")).isFalse();
        assertThat(poll.has("end")).isFalse();
    }

    @Test
    void getProjectionN() {
        var rr = given().
                when()
                .param(RestApi.Param.FIELDS, "id,name")
                .param(RestApi.Param.PAGE_SIZE, 1)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        var polls = bodyAsJSONObject(rr).getJSONObject("_embedded").getJSONArray("pollList");
        assertThat(polls.length()).isEqualTo(1);
        var poll = polls.getJSONObject(0);
        assertThat(poll.has("id")).isTrue();
        assertThat(poll.has("name")).isTrue();
        assertThat(poll.has("description")).isFalse();
        assertThat(poll.has("status")).isFalse();
        assertThat(poll.has("multiOption")).isFalse();
        assertThat(poll.has("start")).isFalse();
        assertThat(poll.has("end")).isFalse();
    }

    @Test
    void getPage_1_7() {
        var rr = given().
                when()
                .param(RestApi.Param.PAGE_NO, 1)
                .param(RestApi.Param.PAGE_SIZE, 7)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        var xTotalCount = rr.extract().header(RestApi.Header.X_TOTAL_COUNT);
        assertThat(DUMMY_POLL_NO).isLessThanOrEqualTo(Integer.parseInt(xTotalCount));
        var xTotalPages = rr.extract().header(RestApi.Header.X_TOTAL_PAGES);
        assertThat(5).isLessThanOrEqualTo(Integer.parseInt(xTotalCount));
        var polls = bodyAsJSONObject(rr).getJSONObject("_embedded").getJSONArray("pollList");
        assertThat(polls.length()).isGreaterThanOrEqualTo(7);
    }

    @Test
    void getPageSize_5() {
        var rr = given().
                when()
                .param(RestApi.Param.PAGE_SIZE, 5)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        var xTotalCount = rr.extract().header(RestApi.Header.X_TOTAL_COUNT);
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
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
    }

}
