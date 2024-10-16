package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.dto.Poll;
import ro.yugori.yadvs.api.model.PollStatus;

import java.time.LocalDateTime;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static ro.yugori.yadvs.api.util.LocalDateTimeMatchers.sameAs;

public class PollCrudTest extends PollBaseTest {

    @Test
    void put() {
        var poll = nextPoll();
        poll.setStart(LocalDateTime.now());

        var rr = createPoll(poll);
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);

        poll.setStart(null);

        rr = given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(poll)
                .put(pollUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .body("name", is(poll.getName()))
                .body("description", is(poll.getDescription()))
                .body("status", is(PollStatus.DRAFT.name()))
                .body("multiOption", is(poll.getMultiOption()))
                .body("start", nullValue())
                .body("end", nullValue());
        assertThat(pollId).isEqualTo(Long.parseLong(rr.extract().path("id").toString()));
    }

    @Test
    void patch() {
        var poll = nextPoll();
        poll.setStart(LocalDateTime.now());

        var rr = createPoll(poll);
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);

        var pollPatch = new Poll();
        pollPatch.setEnd(LocalDateTime.now());

        rr = given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(pollPatch)
                .patch(pollUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .body("name", is(poll.getName()))
                .body("description", is(poll.getDescription()))
                .body("status", is(PollStatus.DRAFT.name()))
                .body("multiOption", is(poll.getMultiOption()))
                .body("start", sameAs(poll.getStart()))
                .body("end", sameAs(pollPatch.getEnd()));
        assertThat(pollId).isEqualTo(Long.parseLong(rr.extract().path("id").toString()));
    }

    @Test
    void delete() {
        var poll = nextPoll();
        var rr = createPoll(poll);
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);

        given().
                when()
                .get(pollUri).
                then()
                .statusCode(HttpStatus.SC_OK)
                .body("name", is(poll.getName()))
                .body("description", is(poll.getDescription()))
                .body("status", is(PollStatus.DRAFT.name()))
                .body("multiOption", is(poll.getMultiOption()));

        given().
                when()
                .delete(pollUri).
                then()
                .statusCode(HttpStatus.SC_NO_CONTENT);

        given().
                when()
                .get(pollUri).
                then()
                .statusCode(HttpStatus.SC_NOT_FOUND);

        given().
                when()
                .delete(pollUri).
                then()
                .statusCode(HttpStatus.SC_NOT_FOUND);

        given().
                when()
                .get(pollUri).
                then()
                .statusCode(HttpStatus.SC_NOT_FOUND);
    }

}
