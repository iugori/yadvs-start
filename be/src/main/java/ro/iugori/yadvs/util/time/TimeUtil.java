package ro.iugori.yadvs.util.time;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

public class TimeUtil {

    private static final DateTimeFormatter TS_ISO_8601 = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static final DateTimeFormatter TS_ISO_8601_Z = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");

    public static String toIsoTs(LocalDateTime v) {
        return v.format(TS_ISO_8601);
    }

    public static String toIsoZoneTs(ZonedDateTime v) {
        return v.format(TS_ISO_8601_Z);
    }

    public static String toIsoDefaultZoneTs(LocalDateTime v) {
        return toIsoZoneTs(toDefaultZone(v));
    }

    private static ZonedDateTime toDefaultZone(LocalDateTime v) {
        return v.atZone(ZoneId.systemDefault());
    }

    public static ZonedDateTime toZoneOffsetUTC(LocalDateTime v) {
        return v.atZone(ZoneId.systemDefault()).withZoneSameInstant(ZoneId.of("UTC"));
    }

    public static LocalDateTime nowUTC() {
        return LocalDateTime.now(ZoneOffset.UTC);
    }

}
