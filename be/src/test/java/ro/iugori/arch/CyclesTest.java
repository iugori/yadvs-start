package ro.iugori.arch;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.library.dependencies.SlicesRuleDefinition.slices;

@AnalyzeClasses(packages = "ro.iugori.yadvs", importOptions = {ImportOption.DoNotIncludeTests.class})
public class CyclesTest {

    @ArchTest
    static ArchRule CYCLES = slices()
            .matching("ro.iugori.yadvs.(**)")
            .should().beFreeOfCycles();

}
