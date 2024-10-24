package ro.yugori.yadvs.api.poll;

import com.github.javafaker.Faker;
import io.restassured.response.ValidatableResponse;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.AfterAll;
import ro.yugori.yadvs.api.ApiTest;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.Setup;
import ro.yugori.yadvs.api.dto.Option;
import ro.yugori.yadvs.api.dto.Poll;
import ro.yugori.yadvs.api.model.PollStatus;

import java.util.ArrayList;
import java.util.List;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static ro.yugori.yadvs.api.util.LocalDateTimeMatchers.sameAs;

public class PollBaseTest extends ApiTest {

    public static final String POLLS_URI = Setup.ROOT_REST_URI + "/polls";
    public static final String OPTIONS_PATH = "/options";

    protected static final Faker FAKER = new Faker();

    private static final List<Long> TEST_POLL_IDS = new ArrayList<>();

    public static long parsePollId(String pollUri) {
        return Long.parseLong(pollUri.substring(pollUri.lastIndexOf("/") + 1));
    }

    public static String buildPollUri(long pollId) {
        return String.format("%s/%d", POLLS_URI, pollId);
    }

    public static String buildPollOptionsUri(long pollId) {
        return String.format("%s%s", buildPollUri(pollId), OPTIONS_PATH);
    }

    public static Poll nextPoll() {
        return Poll.builder()
                .name(FAKER.name().title())
                .description(FAKER.lorem().sentence(20))
                .multiOption(FAKER.bool().bool())
//              .start(FAKER.date().past(10, TimeUnit.DAYS).toInstant().atZone(ZoneId.of("UTC")).toLocalDateTime())
//              .end(FAKER.date().future(10, TimeUnit.DAYS).toInstant().atZone(ZoneId.of("UTC")).toLocalDateTime())
                .build();
    }

    public static Option nextOption() {
        return Option.builder()
                .index((short) FAKER.number().numberBetween(0, 1000))
                .description(FAKER.lorem().sentence(20))
                .build();
    }

    public static ValidatableResponse createPoll(Poll poll) {
        var rr = given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(poll)
                .post(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .header(HttpHeaders.LOCATION, containsString(POLLS_URI))
                .header(RestApi.Header.PREFERENCE_APPLIED, blankOrNullString())
                .body("id", notNullValue())
                .body("name", is(poll.getName()))
                .body("description", is(poll.getDescription()))
                .body("status", is(PollStatus.DRAFT.name()))
                .body("multiOption", is(poll.getMultiOption()))
                .body("start", sameAs(poll.getStart()))
                .body("end", sameAs(poll.getEnd()));
        registerPollIdForDeletion(parsePollId(rr.extract().header(HttpHeaders.LOCATION)));
        return rr;
    }

    public static void deletePoll(long id) {
        given().
                when()
                .delete(buildPollUri(id)).
                then()
                .statusCode(in(List.of(HttpStatus.SC_NO_CONTENT, HttpStatus.SC_NOT_FOUND)))
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
    }

    protected static void registerPollIdForDeletion(long pollId) {
        TEST_POLL_IDS.add(pollId);
    }

    @AfterAll
    static void cleanDatabase() {
        TEST_POLL_IDS.forEach(PollBaseTest::deletePoll);
    }


    public static void main(String... args) {

    }

}
