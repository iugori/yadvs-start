package ro.iugori.yadvs.util.text;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.regex.Pattern;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class TextUtil {

    private static final Pattern IDENTIFIER_PATTERN = Pattern.compile("[a-zA-Z_$][a-zA-Z0-9_$]*");

    public static boolean isValidIdentifier(String source) {
        return IDENTIFIER_PATTERN.matcher(source).matches();
    }

}
