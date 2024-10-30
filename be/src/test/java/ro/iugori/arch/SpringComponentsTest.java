package ro.iugori.arch;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;

@AnalyzeClasses(packages = "ro.iugori.yadvs", importOptions = {ImportOption.DoNotIncludeTests.class})
public class SpringComponentsTest {

    @ArchTest
    static final ArchRule ENTITY_CLASSES = classes().that()
            .areAnnotatedWith(jakarta.persistence.Entity.class)
            .should().resideInAPackage("..model.entity")
            .andShould().haveSimpleNameEndingWith("Entity");

    @ArchTest
    static final ArchRule REPOSITORY_CLASSES = classes().that()
            .areAnnotatedWith(org.springframework.stereotype.Repository.class)
            .should().resideInAPackage("..repository..")
            .andShould().haveSimpleNameEndingWith("Repository")
            .orShould().haveSimpleNameEndingWith("RepositoryImpl");

    @ArchTest
    static final ArchRule SERVICE_CLASSES = classes().that()
            .areAnnotatedWith(org.springframework.stereotype.Service.class)
            .should().resideInAPackage("..service.api")
            .andShould().haveSimpleNameEndingWith("Service");

    @ArchTest
    static final ArchRule REST_CONTROLLER_CLASSES = classes().that()
            .areAnnotatedWith(org.springframework.web.bind.annotation.RestController.class)
            .should().resideInAPackage("..web.rest.api")
            .andShould().haveSimpleNameEndingWith("Resource");

}
