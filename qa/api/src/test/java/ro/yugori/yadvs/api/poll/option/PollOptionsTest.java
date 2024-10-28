package ro.yugori.yadvs.api.poll.option;

import io.restassured.filter.log.RequestLoggingFilter;
import io.restassured.filter.log.ResponseLoggingFilter;
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

        given().filters(new RequestLoggingFilter(), new ResponseLoggingFilter()).
                when()
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
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(1))
                .body("optionList[0].position", is(1))
                .body("optionList[0].description", is(options.getFirst().getDescription()));

        options.add(nextOption());
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of("optionList", options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(2));
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(2))
                .body("optionList[0].position", is(1))
                .body("optionList[0].description", is(options.get(0).getDescription()))
                .body("optionList[1].position", is(2))
                .body("optionList[1].description", is(options.get(1).getDescription()));

        options.get(0).setPosition(null);
        options.get(1).setPosition((short) 2);
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of("optionList", options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(2));
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(2))
                .body("optionList[0].position", is(1))
                .body("optionList[0].description", is(options.get(1).getDescription()))
                .body("optionList[1].position", is(2))
                .body("optionList[1].description", is(options.get(0).getDescription()));

        options.remove(0);
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of("optionList", options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(1));
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("optionList", hasSize(1))
                .body("optionList[0].position", is(1))
                .body("optionList[0].description", is(options.getFirst().getDescription()));

        options.remove(0);
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of("optionList", options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of("optionList", options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
    }

}
