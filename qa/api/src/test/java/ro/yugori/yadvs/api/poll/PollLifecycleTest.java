package ro.yugori.yadvs.api.poll;

import io.restassured.filter.log.RequestLoggingFilter;
import io.restassured.filter.log.ResponseLoggingFilter;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static ro.yugori.yadvs.api.poll.PollTesting.*;

public class PollLifecycleTest {

    @Test
    void happyPath1() {
        var rr = createPoll(PollTesting.nextPoll());
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);

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
