package ro.iugori.yadvs.model.rest.sturctured;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@NoArgsConstructor
@Getter
@Setter
public class BasicPollReport {

    private String title;
    private String description;
    private List<Object[]> options;

}
