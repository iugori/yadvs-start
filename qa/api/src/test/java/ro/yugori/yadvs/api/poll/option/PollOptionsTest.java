package ro.yugori.yadvs.api.poll.option;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.dto.PollOption;
import ro.yugori.yadvs.api.poll.PollBaseTest;

import java.util.ArrayList;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;

public class PollOptionsTest extends PollBaseTest {

    @Test
    void crud() {
        var poll = nextPoll();
        var rr = createPoll(poll);
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollOptionsUri = buildPollOptionsUri(pollId);

        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        var options = new ArrayList<PollOption>();
        options.add(nextOption());

        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of("optionList", options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(1))
                .body("optionList[0].position", is(1))
                .body("optionList[0].description", is(options.getFirst().getDescription()));
    }

}
