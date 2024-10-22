package ro.iugori.arch;

import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition;
import org.junit.jupiter.api.Test;

public class SpringComponentsNamingTest {

    private final JavaClasses APP_CLASSES = new ClassFileImporter()
            .withImportOption(ImportOption.Predefined.DO_NOT_INCLUDE_TESTS)
            .importPackages("ro.iugori.yadvs");

    @Test
    void entityClasses() {
        ArchRuleDefinition.classes().that()
                .areAnnotatedWith(jakarta.persistence.Entity.class)
                .should().resideInAPackage("ro.iugori.yadvs.model.entity")
                .andShould().haveSimpleNameEndingWith("Entity")
                .check(APP_CLASSES);
    }

    @Test
    void serviceClasses() {
        ArchRuleDefinition.classes().that()
                .areAnnotatedWith(org.springframework.stereotype.Service.class)
                .should().resideInAPackage("ro.iugori.yadvs.service")
                .andShould().haveSimpleNameEndingWith("Service")
                .check(APP_CLASSES);
    }

    @Test
    void restControllerClasses() {
        ArchRuleDefinition.classes().that()
                .areAnnotatedWith(org.springframework.web.bind.annotation.RestController.class)
                .should().resideInAPackage("ro.iugori.yadvs.web.rest")
                .andShould().haveSimpleNameEndingWith("Resource")
                .check(APP_CLASSES);
    }

}
