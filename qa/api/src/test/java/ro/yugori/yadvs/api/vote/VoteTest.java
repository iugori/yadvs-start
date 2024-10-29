package ro.yugori.yadvs.api.vote;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.dto.Vote;
import ro.yugori.yadvs.api.poll.PollTesting;

import java.util.List;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static ro.yugori.yadvs.api.ApiTest.bodyAsJSONObject;
import static ro.yugori.yadvs.api.option.PollOptionsTesting.*;
import static ro.yugori.yadvs.api.poll.PollTesting.*;
import static ro.yugori.yadvs.api.vote.VoteTesting.VOTES_URI;

public class VoteTest {

    @Test
    void voteForActivePoll() {
        var rr = createPoll(PollTesting.nextPoll());
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);
        var pollOptionsUri = buildPollOptionsUri(pollId);

        var options = List.of(nextOption(), nextOption(), nextOption());
        rr = given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body(OPTION_LIST, hasSize(3));

        var jsonBody = bodyAsJSONObject(rr);
        var optionId = jsonBody.getJSONArray(OPTION_LIST).getJSONObject(0).getLong("id");

        given().when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is("ACTIVE"));

        given().when()
                .contentType(MimeType.Application.JSON)
                .body(new Vote(optionId))
                .post(VOTES_URI).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
    }

}
