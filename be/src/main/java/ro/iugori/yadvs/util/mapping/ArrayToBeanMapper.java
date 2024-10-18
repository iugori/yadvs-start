package ro.iugori.yadvs.util.mapping;

import lombok.SneakyThrows;

import java.lang.reflect.Field;
import java.util.ArrayList;

public class ArrayToBeanMapper<B> {

    private final Class<B> beanClazz;
    private final Field[] beanFields;

    public static <T> ArrayToBeanMapper<T> of(Class<T> beanClazz, Iterable<String> fields) {
        var beanFields = new ArrayList<Field>();
        for (var fieldName : fields) {
            Field field = null;
            var clazz = (Class<?>) beanClazz;
            while (clazz != null) {
                try {
                    field = clazz.getDeclaredField(fieldName);
                    field.setAccessible(true);
                    break;
                } catch (NoSuchFieldException e) {
                    clazz = clazz.getSuperclass();
                }
            }
            beanFields.add(field);
        }
        return new ArrayToBeanMapper<T>(beanClazz, beanFields.toArray(new Field[]{}));
    }

    private ArrayToBeanMapper(Class<B> beanClazz, Field[] beanFields) {
        this.beanClazz = beanClazz;
        this.beanFields = beanFields;
    }

    @SneakyThrows
    public B map(Object[] array) {
        var bean = beanClazz.getConstructor().newInstance();
        for (int i = 0; i < beanFields.length; ++i) {
            beanFields[i].set(bean, array[i]);
        }
        return bean;
    }

}
