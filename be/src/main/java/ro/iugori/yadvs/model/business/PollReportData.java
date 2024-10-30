package ro.iugori.yadvs.model.business;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@NoArgsConstructor
@Getter
@Setter
public class PollReportData {

    private String title;
    private String description;
    private List<Object[]> options;

}
