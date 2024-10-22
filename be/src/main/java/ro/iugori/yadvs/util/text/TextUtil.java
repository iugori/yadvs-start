package ro.iugori.yadvs.util.text;

import java.util.regex.Pattern;

public class TextUtil {

    private static final Pattern IDENTIFIER_PATTERN = Pattern.compile("[a-zA-Z_$][a-zA-Z0-9_$]*");

    public static boolean isValidIdentifier(String source) {
        return IDENTIFIER_PATTERN.matcher(source).matches();
    }

}
