package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.MoreHttpHeaders;
import ro.yugori.yadvs.api.RestApi;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;

public class PollPreferTest extends PollBaseTest {

    @Test
    void preferMinimal() {
        var poll = nextPoll();
        var rr = given().
                when()
                .contentType(MimeType.Application.JSON)
                .header(RestApi.Header.PREFER, RestApi.Header.Value.RETURN_MINIMAL)
                .body(poll)
                .post(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .header(HttpHeaders.LOCATION, containsString(POLLS_URI))
                .header(RestApi.Header.PREFERENCE_APPLIED, is(RestApi.Header.Value.RETURN_MINIMAL));
        registerPollIdForDeletion(parsePollId(rr.extract().header(HttpHeaders.LOCATION)));
    }

    @Test
    void preferRepresentation() {
        var poll = nextPoll();
        var rr = given().
                when()
                .contentType(MimeType.Application.JSON)
                .header(RestApi.Header.PREFER, RestApi.Header.Value.RETURN_REPRESENTATION)
                .body(poll)
                .post(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .header(HttpHeaders.LOCATION, containsString(POLLS_URI))
                .header(RestApi.Header.PREFERENCE_APPLIED, is(RestApi.Header.Value.RETURN_REPRESENTATION));
        registerPollIdForDeletion(parsePollId(rr.extract().header(HttpHeaders.LOCATION)));
    }

}
