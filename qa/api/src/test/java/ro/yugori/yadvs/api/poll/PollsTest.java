package ro.yugori.yadvs.api.poll;

import com.github.javafaker.Faker;
import io.restassured.RestAssured;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import ro.yugori.yadvs.api.Setup;
import ro.yugori.yadvs.api.dto.Poll;

import java.time.LocalDateTime;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class PollsTest {

    public static final String RESOURCE_URI = Setup.ROOT_REST_URI + "/polls";

    protected static final Faker FAKER = new Faker();

    @BeforeAll
    public static void setup() {
        RestAssured.baseURI = Setup.BASE_URL;
        // Add additional configurations if needed
    }







    @Test
    void getPollsNoneFound() {
        given().baseUri(Setup.BASE_URL)
                .when().get(RESOURCE_URI)
                .then().statusCode(HttpStatus.SC_NO_CONTENT);
    }

}
