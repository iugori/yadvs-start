package ro.yugori.yadvs.api;

import java.util.stream.IntStream;

import static ro.yugori.yadvs.api.poll.PollTesting.createPoll;
import static ro.yugori.yadvs.api.poll.PollTesting.nextPoll;

public class DbInitializer {

    public static void main(String... args) {
        IntStream.range(0, 10).forEach(i -> createPoll(nextPoll()));
        System.out.println("Done.");
    }

}
