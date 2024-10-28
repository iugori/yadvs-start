package ro.yugori.yadvs.api;

import io.restassured.RestAssured;
import io.restassured.response.ValidatableResponse;
import org.apache.commons.lang3.StringUtils;
import org.json.JSONObject;

public abstract class ApiTest {

    static {
        RestAssured.baseURI = Setup.BASE_URL;
        // RestAssured.filters(new RequestLoggingFilter(), new ResponseLoggingFilter());
    }

    public static JSONObject bodyAsJSONObject(ValidatableResponse r) {
        var bodyJson = r.extract().body().asString();
        return StringUtils.isEmpty(bodyJson) ? new JSONObject() : new JSONObject(bodyJson);
    }

}
