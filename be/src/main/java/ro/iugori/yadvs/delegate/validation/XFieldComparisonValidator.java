package ro.iugori.yadvs.delegate.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.SneakyThrows;
import org.apache.commons.lang3.StringUtils;
import ro.iugori.yadvs.util.conversion.ConversionUtil;
import ro.iugori.yadvs.util.reflection.ReflectionUtil;

import java.math.BigDecimal;
import java.util.List;
import java.util.Locale;

public class XFieldComparisonValidator implements ConstraintValidator<XFieldComparison, Object> {

    private static final List<String> VALID_COMPARISONS = List.of("eq", "ne", "lt", "le", "ge", "gt");

    private String rel;
    private String f1;
    private String f2;

    @Override
    public void initialize(XFieldComparison constraintAnnotation) {
        rel = StringUtils.trimToEmpty(constraintAnnotation.rel()).toLowerCase(Locale.ROOT);
        if (!VALID_COMPARISONS.contains(rel)) {
            throw new IllegalArgumentException(String.format("Unsupported relation `%s' for %s validator",
                    rel, XFieldComparison.class.getSimpleName()));
        }
        f1 = constraintAnnotation.field1();
        f2 = constraintAnnotation.field2();
    }

    @Override
    @SneakyThrows
    @SuppressWarnings("unchecked")
    public boolean isValid(Object value, ConstraintValidatorContext context) {
        var valueType = value.getClass();

        var field1 = ReflectionUtil.getDeclaredField(valueType, f1)
                .orElseThrow(() -> new IllegalArgumentException(String.format("Field `%s' does not exist in class %s validator", f1, valueType.getName())));
        var field2 = ReflectionUtil.getDeclaredField(valueType, f2)
                .orElseThrow(() -> new IllegalArgumentException(String.format("Field `%s' does not exist in class %s validator", f2, valueType.getName())));

        field1.setAccessible(true);
        var v1 = field1.get(value);
        if (v1 == null) {
            return true;
        }
        var type1 = field1.getType();

        field2.setAccessible(true);
        var v2 = field2.get(value);
        if (v2 == null) {
            return true;
        }
        var type2 = field2.getType();

        Integer cmpResult = null;

        if (Comparable.class.isAssignableFrom(type1) && type1.isAssignableFrom(type2)) {
            cmpResult = ((Comparable<Object>) v1).compareTo(v2);
        } else if (Comparable.class.isAssignableFrom(type2) && type2.isAssignableFrom(type1)) {
            cmpResult = -((Comparable<Object>) v2).compareTo(v1);
        } else if (v1 instanceof Number && v2 instanceof Number) {
            v1 = ConversionUtil.as(BigDecimal.class, v1);
            v2 = ConversionUtil.as(BigDecimal.class, v2);
            cmpResult = ((BigDecimal) v1).compareTo((BigDecimal) v2);
        } else if (ConversionUtil.isBoolean(v1) && ConversionUtil.isBoolean(v2)) {
            cmpResult = ConversionUtil.as(Boolean.class, v1).compareTo(ConversionUtil.as(Boolean.class, v2));
        }

        if (cmpResult == null) {
            throw new IllegalArgumentException(String.format("Comparison is undefined for types %s and %s.",
                    type1.getName(), type2.getName()));
        }

        return resolveValidation(cmpResult);
    }

    private boolean resolveValidation(int cmpResult) {
        return switch (rel) {
            case "eq" -> cmpResult == 0;
            case "ne" -> cmpResult != 0;
            case "lt" -> cmpResult < 0;
            case "le" -> cmpResult <= 0;
            case "ge" -> cmpResult >= 0;
            case "gt" -> cmpResult > 0;
            default -> {
                throw new IllegalArgumentException(String.format("Unsupported relation `%s' for %s validator",
                        rel, XFieldComparison.class.getSimpleName()));
            }
        };
    }

}
