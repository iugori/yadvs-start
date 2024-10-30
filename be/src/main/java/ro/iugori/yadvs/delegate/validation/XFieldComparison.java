package ro.iugori.yadvs.delegate.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Constraint(validatedBy = {XFieldComparisonValidator.class})
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface XFieldComparison {

    String field1();

    String rel();

    String field2();

    String message() default "Fields are not in the specified relation";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

}
