package ro.yugori.yadvs.api;

import io.restassured.RestAssured;

public abstract class ApiTest {

    static {
        RestAssured.baseURI = Setup.BASE_URL;
    }

}
