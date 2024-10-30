package ro.iugori.arch;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;

@AnalyzeClasses(packages = "ro.iugori.yadvs", importOptions = {ImportOption.DoNotIncludeTests.class})
public class PackageDependencyTest {

    @ArchTest
    static ArchRule WEB_REST = noClasses().that().resideInAPackage("..web.rest..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("..repository..");

    @ArchTest
    static ArchRule SERVICE = noClasses().that().resideInAPackage("..service..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("..web..", "..repository.impl");

    @ArchTest
    static ArchRule REPOSITORY = noClasses().that().resideInAPackage("..repository..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("..web..", "..service..");

}
