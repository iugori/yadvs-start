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
            public boolean matches(Object actual) {
                if (expected == null && actual == null) {
                    return true;
                }
                if (expected == null || actual == null) {
                    return false;
                }
                actual = LocalDateTime.parse(String.valueOf(actual));
                var delta = Duration.between(expected, (LocalDateTime) actual);
                return Math.abs(delta.getNano()) < 1_000_000;
            }

        };
    }

}
