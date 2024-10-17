package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestRequests;
import ro.yugori.yadvs.api.dto.Poll;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;

public class PollErrorsTest extends PollBaseTest {

    @Test
    void getParseError() {
        given().
                when()
                .param(RestRequests.Params.FIELDS, "a+b")
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors[0].message", is("Cannot use projection field `a+b' (must be a valid Java identifier)."));
    }

    @Test
    void postNoNameOrDescription() {
        given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(new Poll())
                .post(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors", hasSize(4))
                .body("trace", notNullValue());
    }

    @Test
    void postDuplicateName() {
        var poll = nextPoll();
        createPoll(poll);
        given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(poll)
                .post(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors", hasSize(1))
                .body("errors[0].message", is("Poll.name must be unique"));
    }

    @Test
    void putDuplicateName() {
        var poll1 = nextPoll();
        createPoll(poll1);

        var poll2 = nextPoll();
        var poll2Uri = createPoll(poll2).extract().header(HttpHeaders.LOCATION);

        poll2.setName(poll1.getName());

        given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(poll2)
                .put(poll2Uri).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors", hasSize(1))
                .body("errors[0].message", is("Poll.name must be unique"));
    }

    @Test
    void patchDuplicateName() {
        var poll1 = nextPoll();
        createPoll(poll1);

        var poll2 = nextPoll();
        var poll2Uri = createPoll(poll2).extract().header(HttpHeaders.LOCATION);

        var poll = new Poll();
        poll.setName(poll1.getName());

        given().
                when()
                .contentType(MimeType.Application.JSON)
                .body(poll)
                .patch(poll2Uri).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors", hasSize(1))
                .body("errors[0].message", is("Poll.name must be unique"));
    }

    @Test
    void methodNotAllowed() {
        given().
                when()
                .put(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_METHOD_NOT_ALLOWED);

        given().
                when()
                .patch(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_METHOD_NOT_ALLOWED);

        given().
                when()
                .delete(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_METHOD_NOT_ALLOWED);
    }

}
