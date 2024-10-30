package ro.iugori.arch;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.core.importer.Location;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;
import ro.iugori.yadvs._start.YadvsApplication;

import static com.tngtech.archunit.library.Architectures.layeredArchitecture;

class DoNotIncludeMainClass implements ImportOption {

    private static final String MAIN_CLASS_NAME = YadvsApplication.class.getSimpleName();

    @Override
    public boolean includes(Location location) {
        return !location.contains(MAIN_CLASS_NAME);
    }

}

@AnalyzeClasses(packages = "ro.iugori.yadvs", importOptions = {ImportOption.DoNotIncludeTests.class, DoNotIncludeMainClass.class})
public class LayersTest {

    @ArchTest
    static ArchRule LAYERS = layeredArchitecture()
            .consideringAllDependencies()
            .layer("Controller").definedBy("..web.rest..")
            .layer("Service").definedBy("..service..")
            .layer("Repository").definedBy("..repository..")

            .whereLayer("Controller").mayNotBeAccessedByAnyLayer()
            .whereLayer("Service").mayOnlyBeAccessedByLayers("Controller")
            .whereLayer("Repository").mayOnlyBeAccessedByLayers("Service");

}
