package ro.yugori.yadvs.api.poll;

import lombok.val;
import org.apache.http.HttpHeaders;
import org.junit.jupiter.api.Test;

import java.util.List;

public class PollOptionsTest extends PollBaseTest {

    @Test
    void crud() {
        var poll = nextPoll();
        var rr = createPoll(poll);
        var pollId = parsePollId(rr.extract().header(HttpHeaders.LOCATION));
        var pollUri = buildPollUri(pollId);
        var pollOptionsUri = buildPollOptionsUri(pollId);
        var options = List.of(nextOption(), nextOption(), nextOption());

    }

}
