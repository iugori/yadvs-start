package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static ro.yugori.yadvs.api.poll.PollTesting.POLLS_URI;
import static ro.yugori.yadvs.api.poll.PollTesting.nextPoll;

public class PollPreferTest {

    @Test
    void preferMinimal() {
        given().
                when()
                .contentType(MimeType.Application.JSON)
                .header(RestApi.Header.PREFER, RestApi.Header.Value.RETURN_MINIMAL)
                .body(nextPoll())
                .post(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .header(HttpHeaders.LOCATION, containsString(POLLS_URI))
                .header(RestApi.Header.PREFERENCE_APPLIED, is(RestApi.Header.Value.RETURN_MINIMAL));
    }

    @Test
    void preferRepresentation() {
        given().
                when()
                .contentType(MimeType.Application.JSON)
                .header(RestApi.Header.PREFER, RestApi.Header.Value.RETURN_REPRESENTATION)
                .body(nextPoll())
                .post(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .header(HttpHeaders.LOCATION, containsString(POLLS_URI))
                .header(RestApi.Header.PREFERENCE_APPLIED, is(RestApi.Header.Value.RETURN_REPRESENTATION));
    }

}
