package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.MimeType;
import ro.yugori.yadvs.api.RestApi;
import ro.yugori.yadvs.api.dto.Poll;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static ro.yugori.yadvs.api.poll.PollTesting.*;

public class PollErrorsTest {

    @Test
    void getInvalidPollId() {
        var pollUri = buildPollUri("un-parsable");
        given().
                when()
                .get(pollUri).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("2101: type_conversion"));
    }

    @Test
    void getProjectionError() {
        given().
                when()
                .param(RestApi.Param.FIELDS, "a+b")
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("3210: projection_criteria"))
                .body("_embedded.errors[0].message", is("Cannot use projection field `a+b' (must be a valid Java identifier)."));
    }

    @Test
    void getSelectionError() {
        given().
                when()
                .param("name~x", "a")
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("3220: selection_criteria"))
                .body("_embedded.errors[0].message", is("Cannot parse selection predicate operator `x'."));
    }

    @Test
    void getSortingError() {
        given().
                when()
                .param(RestApi.Param.SORT, "a, +!a")
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("3230: sorting_criteria"))
                .body("_embedded.errors[0].message", is("Cannot parse sort field `!a' (must be a valid Java identifier)."));
    }

    @Test
    void getPaginationPageNoError() {
        given().
                when()
                .param(RestApi.Param.PAGE_NO, 0)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("3240: pagination_criteria"))
                .body("_embedded.errors[0].message", is("Must be greater than or equal to 1."));
    }

    @Test
    void getPaginationPageSizeError() {
        given().
                when()
                .param(RestApi.Param.PAGE_SIZE, 0)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("3240: pagination_criteria"))
                .body("_embedded.errors[0].message", is("Must be greater than or equal to 1."));
    }

    @Test
    void getPaginationPageNoWithoutPageSizeError() {
        given().
                when()
                .param(RestApi.Param.PAGE_NO, 1)
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_BAD_REQUEST)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors[0].code", is("3240: pagination_criteria"))
                .body("_embedded.errors[0].message", is("Cannot have `~pageNo' without `~pageSize'."));
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
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors", hasSize(4))
                .body("logref", notNullValue());
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
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors", hasSize(1))
                .body("_embedded.errors[0].code", is("2102: resource_conflict"))
                .body("_embedded.errors[0].message", is("Poll.name must be unique"));
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
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors", hasSize(1))
                .body("_embedded.errors[0].code", is("2102: resource_conflict"))
                .body("_embedded.errors[0].message", is("Poll.name must be unique"));
    }

    @Test
    void patchJsonPatchJson() {
        var poll = nextPoll();
        var pollUri = createPoll(poll).extract().header(HttpHeaders.LOCATION);
        given().
                when()
                .contentType(MimeType.Application.JSON_PATCH_JSON)
                .body(poll)
                .patch(pollUri).
                then()
                .statusCode(HttpStatus.SC_UNSUPPORTED_MEDIA_TYPE)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors", hasSize(1))
                .body("_embedded.errors[0].code", is("1010: api_error"))
                .body("_embedded.errors[0].message", is("Content-Type 'application/json-patch+json;charset=ISO-8859-1' is not supported"));
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
                .statusCode(HttpStatus.SC_CONFLICT)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("_embedded.errors", hasSize(1))
                .body("_embedded.errors[0].code", is("2102: resource_conflict"))
                .body("_embedded.errors[0].message", is("Poll.name must be unique"));
    }

    @Test
    void methodNotAllowed() {
        given().
                when()
                .put(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_METHOD_NOT_ALLOWED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue())
                .body("status", is(HttpStatus.SC_METHOD_NOT_ALLOWED));

        given().
                when()
                .patch(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_METHOD_NOT_ALLOWED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());

        given().
                when()
                .delete(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_METHOD_NOT_ALLOWED)
                .header(RestApi.Header.X_CORRELATION_ID, notNullValue());
    }

}
