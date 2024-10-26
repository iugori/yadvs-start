package ro.yugori.yadvs.api.poll.option;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.poll.PollBaseTest;

import java.util.List;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;

public class PollOptionsErrorsTest extends PollBaseTest {

    @Test
    void getInvalidPoll() {
        var pollOptionsUri = String.format("%s/%s%s", POLLS_URI, "un-parsable", OPTIONS_PATH);

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
                .body(Map.of("optionList", List.of(option)))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors.findAll { it.code == '2000: not_null' }", not(empty()));
    }

}
