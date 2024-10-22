package ro.iugori.yadvs.util.conversion;

import org.springframework.core.convert.support.DefaultConversionService;
import ro.iugori.yadvs.util.conversion.StringToLocalDateTimeConverter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class ConversionUtil {

    private static final DefaultConversionService conversionService = new DefaultConversionService();

    static {
        conversionService.addConverter(new StringToLocalDateTimeConverter());
    }

    public static boolean isBoolean(Object obj) {
        if (obj == null) {
            return false;
        }
        return obj instanceof Boolean;
    }

    public static <T> T as(Class<T> targetType, Object source) {
        return conversionService.convert(source, targetType);
    }

    public static <T> Collection<T> asCollection(Class<T> targetType, Object obj) {
        if (obj == null) {
            return List.of();
        }

        if (obj instanceof String) {
            obj = ((String) obj).split(",");
        }

        if (obj.getClass().isArray()) {
            var values = new ArrayList<T>();
            int length = java.lang.reflect.Array.getLength(obj);
            for (int i = 0; i < length; i++) {
                var elt = java.lang.reflect.Array.get(obj, i);
                values.add(as(targetType, elt));
            }
            return values;
        }

        if (obj instanceof Collection) {
            return ((Collection<?>) obj).stream().map(elt -> as(targetType, elt)).collect(Collectors.toList());
        }

        return List.of(as(targetType, obj));
    }

}
