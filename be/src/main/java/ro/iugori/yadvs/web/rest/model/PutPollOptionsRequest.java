package ro.iugori.yadvs.web.rest.model;

import jakarta.validation.Valid;
import lombok.Getter;
import lombok.Setter;
import ro.iugori.yadvs.model.business.PollOption;

import java.util.List;

public class PutPollOptionsRequest {

    @Getter
    @Setter
    @Valid
    private List<PollOption> optionList;

}
