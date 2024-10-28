package ro.iugori.yadvs.model.rest.sturctured;

import jakarta.validation.Valid;
import lombok.Getter;
import lombok.Setter;
import ro.iugori.yadvs.model.rest.shared.PollOption;

import java.util.List;

public class PutPollOptionsRequest {

    @Getter
    @Setter
    @Valid
    private List<PollOption> optionList;

}
