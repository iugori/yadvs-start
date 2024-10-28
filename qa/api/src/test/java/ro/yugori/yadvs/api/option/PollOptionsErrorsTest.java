package ro.yugori.yadvs.api.option;

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

public class PollOptionsErrorsTest {

    @Test
    void getInvalidPoll() {
        var pollOptionsUri = buildPollOptionsUri("un-parsable");
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("2101: type_conversion"));
    }

    @Test
    void putInvalidOption() {
        var poll = nextPoll();
        var rr = createPoll(poll);
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollOptionsUri = buildPollOptionsUri(pollId);

        var option = nextOption();
        option.setDescription(null);

        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, List.of(option)))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors.findAll { it.code == '2000: not_null' }", not(empty()));
    }

}
