package ro.iugori.yadvs.delegate.reporting;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ro.iugori.yadvs.delegate.reporting.ChartType;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PollReportSpec {

    @Min(16)
    @Max(5120)
    private int width;

    @Min(16)
    @Max(2880)
    private int height;

    @NotNull
    private ChartType chartType;

}
