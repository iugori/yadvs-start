package ro.yugori.yadvs.api.report;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import static ro.yugori.yadvs.api.poll.PollTesting.POLLS_URI;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ReportTesting {

    public static String buildPollReportUri(Object pollId) {
        return String.format("%s/%s/report", POLLS_URI, pollId);
    }


}
