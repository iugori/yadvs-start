package ro.yugori.yadvs.api;

import org.junit.jupiter.api.Test;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.hasSize;

public class PollsTest {

    public static final String RESOURCE_URL = Setup.REST_URL + "/polls";

    @Test
    void getAllMembers() {
        given().baseUri(Setup.BASE_URL)
                .when().get(RESOURCE_URL)
                .then().statusCode(204);
    }

}
