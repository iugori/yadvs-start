package ro.yugori.yadvs.api.option;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import ro.yugori.yadvs.api.dto.PollOption;

import static ro.yugori.yadvs.api.ApiTest.FAKER;
import static ro.yugori.yadvs.api.poll.PollTesting.buildPollUri;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class PollOptionsTesting {

    public static final String OPTIONS_PATH = "/options";

    public static final String OPTION_LIST = "optionList";

    public static String buildPollOptionsUri(Object pollId) {
        return String.format("%s%s", buildPollUri(pollId), OPTIONS_PATH);
    }

    public static PollOption nextOption() {
        return PollOption.builder()
                .description(FAKER.name().fullName())
                .build();
    }

}
