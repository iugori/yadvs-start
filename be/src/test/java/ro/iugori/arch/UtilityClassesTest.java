package ro.iugori.arch;

import com.tngtech.archunit.core.domain.JavaClass;
import com.tngtech.archunit.core.domain.JavaModifier;
import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchCondition;
import com.tngtech.archunit.lang.ArchRule;
import com.tngtech.archunit.lang.ConditionEvents;
import com.tngtech.archunit.lang.SimpleConditionEvent;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;

@AnalyzeClasses(packages = "ro.iugori.yadvs", importOptions = {ImportOption.DoNotIncludeTests.class})
public class UtilityClassesTest {

    @ArchTest
    static final ArchRule UTILITY_CLASSES = classes().that()
            .haveSimpleNameEndingWith("Util")
            .should().haveModifier(JavaModifier.FINAL)
            .andShould().haveOnlyPrivateConstructors()
            .andShould(haveOnlyStaticFieldsAndMethods());

    public static ArchCondition<JavaClass> haveOnlyStaticFieldsAndMethods() {
        return new ArchCondition<>("have only static fields and methods") {
            @Override
            public void check(JavaClass item, ConditionEvents events) {
                item.getMethods().forEach(method -> {
                    var isStatic = method.getModifiers().contains(JavaModifier.STATIC);
                    if (!isStatic) {
                        var message = String.format("Method %s.%s is not static", item.getName(), method.getName());
                        events.add(SimpleConditionEvent.violated(method, message));
                    }
                });
                item.getAllFields().forEach(field -> {
                    var isStatic = field.getModifiers().contains(JavaModifier.STATIC);
                    if (!isStatic) {
                        var message = String.format("Field %s.%s is not static", item.getName(), field.getName());
                        events.add(SimpleConditionEvent.violated(field, message));
                    }
                });
            }
        };
    }

}
