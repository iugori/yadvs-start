package ro.yugori.yadvs.api;

import com.github.javafaker.Faker;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.dto.Poll;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class PollsTest {

    public static final String RESOURCE_URI = Setup.ROOT_REST_URI + "/polls";

    private static final Faker FAKER = new Faker();

    @Test
    void crudPoll() {
        var poll = Poll.builder()
                .name(FAKER.name().title())
                .description(FAKER.lorem().sentence(20))
                .build();

        var rr = given().baseUri(Setup.BASE_URL)
                .when().contentType(MimeType.Application.JSON).body(poll).post(RESOURCE_URI)
                .then().statusCode(HttpStatus.SC_CREATED)
                .header(HttpHeaders.LOCATION, containsString(RESOURCE_URI))
                .header("Preference-Applied", blankOrNullString());

        var bodyStr = rr.extract().response().asString();
        var bodyJson = new JSONObject(bodyStr);

        var pollId = bodyJson.getLong("id");
        var pollUri = rr.extract().header(HttpHeaders.LOCATION);

        assertEquals(RESOURCE_URI + "/" + pollId, pollUri);
        assertEquals(poll.getName(), bodyJson.getString("name"));
        assertEquals(poll.getDescription(), bodyJson.getString("description"));
        assertEquals("DRAFT", bodyJson.getString("status"));

        given().baseUri(Setup.BASE_URL)
                .when().contentType(MimeType.Application.JSON).body(poll).post(RESOURCE_URI)
                .then().statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors", hasSize(1));

        given().baseUri(Setup.BASE_URL)
                .when().get(pollUri)
                .then().statusCode(HttpStatus.SC_OK)
                .body("name", is(poll.getName()))
                .body("description", is(poll.getDescription()))
                .body("status", is("DRAFT"));

        given().baseUri(Setup.BASE_URL)
                .when().delete(pollUri)
                .then().statusCode(HttpStatus.SC_NO_CONTENT);

        given().baseUri(Setup.BASE_URL)
                .when().get(pollUri)
                .then().statusCode(HttpStatus.SC_NOT_FOUND);

        given().baseUri(Setup.BASE_URL)
                .when().delete(pollUri)
                .then().statusCode(HttpStatus.SC_NOT_FOUND);

        given().baseUri(Setup.BASE_URL)
                .when().get(pollUri)
                .then().statusCode(HttpStatus.SC_NOT_FOUND);
    }

    @Test
    void postPollBadRequest() {
        given().baseUri(Setup.BASE_URL)
                .when().contentType(MimeType.Application.JSON).body(new Poll()).post(RESOURCE_URI)
                .then().statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors", hasSize(4))
                .body("trace", notNullValue());
    }

    @Test
    void deletePollBadRequest() {
        given().baseUri(Setup.BASE_URL)
                .when().delete(RESOURCE_URI)
                .then().statusCode(HttpStatus.SC_BAD_REQUEST);
    }

    @Test
    void getPollsNoneFound() {
        given().baseUri(Setup.BASE_URL)
                .when().get(RESOURCE_URI)
                .then().statusCode(HttpStatus.SC_NO_CONTENT);
    }

}
