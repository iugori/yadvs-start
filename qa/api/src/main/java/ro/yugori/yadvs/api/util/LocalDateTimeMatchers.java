package ro.yugori.yadvs.api.util;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;

import java.time.Duration;
import java.time.LocalDateTime;

public class LocalDateTimeMatchers {

    public static Matcher<String> sameAs(final LocalDateTime expected) {
        return new BaseMatcher<>() {

            @Override
            public void describeTo(Description description) {
                description.appendText(expected + " - the expected LocalDateTime ");
            }

            @Override
            public boolean matches(Object actualStr) {
                if (expected == null && actualStr == null) {
                    return true;
                }
                if (expected == null || actualStr == null) {
                    return false;
                }
                var expectedStr = expected.toString();
                if (expectedStr.startsWith((String) actualStr)) {
                    return true;
                }
                var actual = LocalDateTime.parse(String.valueOf(actualStr));
                if (expected.equals(actual)) {
                    return true;
                }
                var delta = Duration.between(expected, actual);
                return Math.abs(delta.getNano()) < 100_000_000;
            }

        };
    }

}
