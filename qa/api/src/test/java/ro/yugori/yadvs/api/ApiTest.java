package ro.yugori.yadvs.api;

import com.github.javafaker.Faker;
import io.restassured.RestAssured;
import io.restassured.response.ValidatableResponse;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.json.JSONObject;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ApiTest {

    public static final Faker FAKER = new Faker();

    static {
        RestAssured.baseURI = Setup.BASE_URL;
        // RestAssured.filters(new RequestLoggingFilter(), new ResponseLoggingFilter());
    }

    public static JSONObject bodyAsJSONObject(ValidatableResponse r) {
        var bodyJson = r.extract().body().asString();
        return StringUtils.isEmpty(bodyJson) ? new JSONObject() : new JSONObject(bodyJson);
    }

}
