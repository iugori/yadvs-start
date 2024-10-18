package ro.yugori.yadvs.api;

import io.restassured.RestAssured;
import io.restassured.response.ValidatableResponse;
import org.json.JSONArray;
import org.json.JSONObject;

public abstract class ApiTest {

    static {
        RestAssured.baseURI = Setup.BASE_URL;
    }

    public static JSONObject bodyAsJSONObject(ValidatableResponse r) {
        return new JSONObject(r.extract().body().asString());
    }

    public static JSONArray bodyAsJSONArray(ValidatableResponse r) {
        return new JSONArray(r.extract().body().asString());
    }

}