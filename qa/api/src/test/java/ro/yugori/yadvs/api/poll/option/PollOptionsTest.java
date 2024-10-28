package ro.yugori.yadvs.api.poll.option;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.dto.PollOption;
import ro.yugori.yadvs.api.poll.PollBaseTest;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.*;

public class PollOptionsTest extends PollBaseTest {

    public static final String OPTION_LIST = "optionList";

    @Test
    void crud() {
        var rr = createPoll(nextPoll());
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
                .body(Map.of(OPTION_LIST, options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(1))
                .body(OPTION_LIST + "[0].position", is(1))
                .body(OPTION_LIST + "[0].description", is(options.getFirst().getDescription()));

        options.add(nextOption());
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(2));
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(2))
                .body(OPTION_LIST + "[0].position", is(1))
                .body(OPTION_LIST + "[0].description", is(options.get(0).getDescription()))
                .body(OPTION_LIST + "[1].position", is(2))
                .body(OPTION_LIST + "[1].description", is(options.get(1).getDescription()));

        options.get(0).setPosition(null);
        options.get(1).setPosition((short) 2);
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(2));
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(2))
                .body(OPTION_LIST + "[0].position", is(1))
                .body(OPTION_LIST + "[0].description", is(options.get(1).getDescription()))
                .body(OPTION_LIST + "[1].position", is(2))
                .body(OPTION_LIST + "[1].description", is(options.get(0).getDescription()));

        options.remove(0);
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(1));
        given().when()
                .get(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(1))
                .body(OPTION_LIST + "[0].position", is(1))
                .body(OPTION_LIST + "[0].description", is(options.getFirst().getDescription()));

        options.remove(0);
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, options))
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
                .body(Map.of(OPTION_LIST, options))
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

    @Test
    void cascadeDeletion() {
        var pollAllOptionsUri = buildPollOptionsUri(RestApi.URI.WILDCARD);

        var rr = given().when()
                .get(pollAllOptionsUri).
                then()
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());


        var bodyJson = bodyAsJSONObject(rr);
        var initialOptionNo = bodyJson.has(OPTION_LIST) ? bodyJson.getJSONArray(OPTION_LIST).length() : 0;

        rr = createPoll(nextPoll());
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);
        var pollOptionsUri = buildPollOptionsUri(pollId);

        var options = List.of(nextOption(), nextOption());
        given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        rr = given().when()
                .get(pollAllOptionsUri).
                then()
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        assertThat(initialOptionNo + options.size())
                .isEqualTo(bodyAsJSONObject(rr).getJSONArray(OPTION_LIST).length());

        given().
                when()
                .delete(pollUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        rr = given().when()
                .get(pollAllOptionsUri).
                then()
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        bodyJson = bodyAsJSONObject(rr);
        var finalOptionNo = bodyJson.has(OPTION_LIST) ? bodyJson.getJSONArray(OPTION_LIST).length() : 0;
        assertThat(initialOptionNo).isEqualTo(finalOptionNo);
    }

}
