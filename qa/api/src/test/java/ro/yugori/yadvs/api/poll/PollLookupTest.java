package ro.yugori.yadvs.api.poll;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.stream.IntStream;

import static io.restassured.RestAssured.given;

public class PollLookupTest extends PollBaseTest {

    @BeforeAll
    static void addSomePolls() {
        IntStream.range(0, 30).forEach(i -> createPoll(nextPoll()));
    }

    @Test
    @Disabled
    void getAll() {
        given().
                when()
                .get(POLLS_URI).
                then()
                .statusCode(HttpStatus.SC_OK);
    }

}
