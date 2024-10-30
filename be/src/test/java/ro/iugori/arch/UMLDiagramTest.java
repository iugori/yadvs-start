package ro.iugori.arch;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;
import org.springframework.core.io.ResourceLoader;

import java.net.URL;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;
import static com.tngtech.archunit.library.plantuml.rules.PlantUmlArchCondition.Configuration.consideringOnlyDependenciesInDiagram;
import static com.tngtech.archunit.library.plantuml.rules.PlantUmlArchCondition.adhereToPlantUmlDiagram;

@AnalyzeClasses(packages = "ro.iugori.yadvs", importOptions = {ImportOption.DoNotIncludeTests.class})
public class UMLDiagramTest {

    private static final URL REFERENCE_DIAGRAM = ResourceLoader.class.getResource("/arch/overview.puml");

    @ArchTest
    static ArchRule UML_COMPLIANCE = classes()
            .should(adhereToPlantUmlDiagram(REFERENCE_DIAGRAM, consideringOnlyDependenciesInDiagram()));

}
