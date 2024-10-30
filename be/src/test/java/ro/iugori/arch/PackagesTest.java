package ro.iugori.arch;

import com.tngtech.archunit.core.domain.JavaClass;
import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchCondition;
import com.tngtech.archunit.lang.ArchRule;
import com.tngtech.archunit.lang.ConditionEvents;
import com.tngtech.archunit.lang.SimpleConditionEvent;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;

@AnalyzeClasses(packages = "ro.iugori.yadvs", importOptions = {ImportOption.DoNotIncludeTests.class})
public class PackagesTest {

    @ArchTest
    static final ArchRule LEAF_PACKAGES = classes().should(beInALeafPackage());

    private static ArchCondition<? super JavaClass> beInALeafPackage() {
        return new ArchCondition<>("be in a leaf package") {

            @Override
            public void check(JavaClass item, ConditionEvents events) {
                if (item.getPackage().getSubpackages().size() > 0) {
                    var message = String.format("Class `%s' should be in a leaf package", item.getName());
                    events.add(SimpleConditionEvent.violated(item, message));
                }
            }

        };
    }

}
