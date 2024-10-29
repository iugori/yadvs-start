package ro.yugori.yadvs.api.poll;

import io.restassured.filter.log.RequestLoggingFilter;
import io.restassured.filter.log.ResponseLoggingFilter;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;

import java.util.List;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static ro.yugori.yadvs.api.option.PollOptionsTesting.*;
import static ro.yugori.yadvs.api.poll.PollTesting.*;

public class PollLifecycleTest {

    @Test
    void happyPath1() {
        var rr = createPoll(PollTesting.nextPoll());
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);

        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, List.of(nextOption())))
                .put(buildPollOptionsUri(pollId)).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(1));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ACTIVE"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + SUSPEND_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("SUSPENDED"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ACTIVE"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + CLOSE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("CLOSED"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ARCHIVE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ARCHIVED"));
    }

    @Test
    void happyPath2() {
        var rr = createPoll(PollTesting.nextPoll());
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);

        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, List.of(nextOption(), nextOption())))
                .put(buildPollOptionsUri(pollId)).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(2));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ACTIVE"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + SUSPEND_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("SUSPENDED"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + CLOSE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("CLOSED"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ARCHIVE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ARCHIVED"));
    }

    @Test
    void invalidTransitions() {
        var rr = createPoll(PollTesting.nextPoll());
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + SUSPEND_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + CLOSE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ARCHIVE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());


        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, List.of(nextOption())))
                .put(buildPollOptionsUri(pollId)).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(1));


        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ACTIVE"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ARCHIVE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());


        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + SUSPEND_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("SUSPENDED"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ARCHIVE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());


        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + CLOSE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("CLOSED"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + SUSPEND_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());


        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ARCHIVE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ARCHIVED"));

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + SUSPEND_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + CLOSE_PATH).
                then()
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
    }

}
