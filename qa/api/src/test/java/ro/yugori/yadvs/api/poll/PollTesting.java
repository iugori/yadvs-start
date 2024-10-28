package ro.yugori.yadvs.api.poll;

import io.restassured.response.ValidatableResponse;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.Setup;
import ro.yugori.yadvs.api.dto.Poll;
import ro.yugori.yadvs.api.model.PollStatus;

import java.util.UUID;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static ro.yugori.yadvs.api.ApiTest.FAKER;
import static ro.yugori.yadvs.api.util.LocalDateTimeMatchers.sameAs;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class PollTesting {

    public static final String POLLS_URI = Setup.ROOT_REST_URI + "/polls";

    public static final String ACTIVATE_PATH = "/activate";

    public static long parsePollId(String pollUri) {
        return Long.parseLong(pollUri.substring(pollUri.lastIndexOf("/") + 1));
    }

    public static String buildPollUri(Object pollId) {
        return String.format("%s/%s", POLLS_URI, pollId);
    }

    public static Poll nextPoll() {
        return Poll.builder()
                .name(FAKER.name().title() + " " + UUID.randomUUID().toString().replace("-", ""))
                .description(FAKER.lorem().sentence(20))
                .multiOption(FAKER.bool().bool())
//              .start(FAKER.date().past(10, TimeUnit.DAYS).toInstant().atZone(ZoneId.of("UTC")).toLocalDateTime())
//              .end(FAKER.date().future(10, TimeUnit.DAYS).toInstant().atZone(ZoneId.of("UTC")).toLocalDateTime())
                .build();
    }


    public static ValidatableResponse createPoll(Poll poll) {
        return given().
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
    }

}
