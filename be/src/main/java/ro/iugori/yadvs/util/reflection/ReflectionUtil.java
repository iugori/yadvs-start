package ro.iugori.yadvs.util.reflection;

import java.lang.reflect.Field;
import java.util.Optional;

public class ReflectionUtil {

    public static Optional<Field> getDeclaredField(Class<?> clazz, String name) {
        Field field = null;
        while (clazz != null) {
            try {
                field = clazz.getDeclaredField(name);
                field.setAccessible(true);
                break;
            } catch (NoSuchFieldException e) {
                clazz = clazz.getSuperclass();
            }
        }
        return field == null ? Optional.empty() : Optional.of(field);
    }

}
