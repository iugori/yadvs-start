package ro.yugori.yadvs.api.report;

import io.restassured.filter.log.RequestLoggingFilter;
import io.restassured.filter.log.ResponseLoggingFilter;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.dto.Poll;
import ro.yugori.yadvs.api.dto.Vote;

import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static ro.yugori.yadvs.api.ApiTest.bodyAsJSONObject;
import static ro.yugori.yadvs.api.option.PollOptionsTesting.*;
import static ro.yugori.yadvs.api.poll.PollTesting.*;
import static ro.yugori.yadvs.api.report.ReportTesting.buildPollReportUri;
import static ro.yugori.yadvs.api.vote.VoteTesting.VOTES_URI;

public class PollReportTest {

    private static Poll POLL;

    @BeforeAll
    static void startPoll() {
        POLL = nextPoll();
        var rr = createPoll(POLL);
        POLL.setId(parsePollId(rr.extract().header(HttpHeaders.LOCATION)));
        var pollUri = buildPollUri(POLL.getId());
        var pollOptionsUri = buildPollOptionsUri(POLL.getId());

        var options = List.of(nextOption(), nextOption(), nextOption());
        rr = given().when()
                .contentType(MimeType.Application.JSON)
                .body(Map.of(OPTION_LIST, options))
                .put(pollOptionsUri).
                then()
                .statusCode(HttpStatus.SC_CREATED)
                .body(OPTION_LIST, hasSize(3));

        var jsonOptions = bodyAsJSONObject(rr).getJSONArray(OPTION_LIST);
        var optionId1 = jsonOptions.getJSONObject(0).getLong("id");
        var optionId2 = jsonOptions.getJSONObject(1).getLong("id");
        var optionId3 = jsonOptions.getJSONObject(2).getLong("id");

        given().when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + ACTIVATE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .body("status", is("ACTIVE"));

        IntStream.range(0, 4).forEach(i -> given().when()
                .contentType(MimeType.Application.JSON)
                .body(new Vote(optionId1))
                .post(VOTES_URI).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT));

        IntStream.range(0, 7).forEach(i -> given().when()
                .contentType(MimeType.Application.JSON)
                .body(new Vote(optionId2))
                .post(VOTES_URI).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT));

        IntStream.range(0, 2).forEach(i -> given().when()
                .contentType(MimeType.Application.JSON)
                .body(new Vote(optionId3))
                .post(VOTES_URI).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT));

        given().when()
                .contentType(MimeType.Application.JSON)
                .patch(pollUri + CLOSE_PATH).
                then()
                .statusCode(HttpStatus.SC_OK)
                .body("status", is("CLOSED"));
    }

    @Test
    void defaultRepresentation() {
        given().when()
                .get(buildPollReportUri(POLL.getId())).
                then()
                .statusCode(HttpStatus.SC_OK)
                .body("options[0][1]", is(4))
                .body("options[1][1]", is(7))
                .body("options[2][1]", is(2));
    }

    @Test
    void imagePng() {
        given().when()
                .header(HttpHeaders.ACCEPT, MimeType.Image.PNG)
                .get(buildPollReportUri(POLL.getId())).
                then()
                .statusCode(HttpStatus.SC_OK)
                .header(HttpHeaders.CONTENT_TYPE, is(MimeType.Image.PNG));
    }

}
