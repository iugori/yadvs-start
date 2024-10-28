package ro.yugori.yadvs.api;

import io.restassured.RestAssured;
import io.restassured.response.ValidatableResponse;
import org.json.JSONObject;

public abstract class ApiTest {

    static {
        RestAssured.baseURI = Setup.BASE_URL;
        // RestAssured.filters(new RequestLoggingFilter(), new ResponseLoggingFilter());
    }

    public static JSONObject bodyAsJSONObject(ValidatableResponse r) {
        return new JSONObject(r.extract().body().asString());
    }

}
