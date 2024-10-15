package ro.yugori.yadvs.api;

import com.github.javafaker.Faker;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.dto.Poll;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;

public class PollsTest {

    public static final String RESOURCE_URL = Setup.REST_URL + "/polls";

    private static final Faker faker = new Faker();

    @Test
    void postPollNull() {
        given().baseUri(Setup.BASE_URL)
                .when().contentType(MimeType.Application.JSON).body(new Poll()).post(RESOURCE_URL)
                .then().statusCode(HttpStatus.SC_BAD_REQUEST)
                .body("errors", hasSize(4))
                .body("trace", notNullValue());
    }

    @Test
    void deletePollInvalidId() {
        given().baseUri(Setup.BASE_URL)
                .when().delete(RESOURCE_URL)
                .then().statusCode(HttpStatus.SC_BAD_REQUEST);
    }

    @Test
    void getPollsNoneFound() {
        given().baseUri(Setup.BASE_URL)
                .when().get(RESOURCE_URL)
                .then().statusCode(HttpStatus.SC_NO_CONTENT);
    }

}
