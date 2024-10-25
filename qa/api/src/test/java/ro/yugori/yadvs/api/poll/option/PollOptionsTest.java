package ro.yugori.yadvs.api.poll.option;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.model.PollStatus;
import ro.yugori.yadvs.api.poll.PollBaseTest;

import java.util.List;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.Matchers.nullValue;

public class PollOptionsTest extends PollBaseTest {

    @Test
    @Disabled
    void crud() {
        var poll = nextPoll();
        var rr = createPoll(poll);
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollOptionsUri = buildPollOptionsUri(pollId);

        given().
                when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        var options = List.of(nextOption(), nextOption(), nextOption());

        rr = given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(options)
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("name", is(poll.getName()));

    }

}
