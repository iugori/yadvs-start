package ro.iugori.yadvs.model.rest.sturctured;

import lombok.Getter;
import org.springframework.hateoas.RepresentationModel;
import ro.iugori.yadvs.model.rest.shared.PollOption;

import java.util.List;

public class PollOptionsResponse extends RepresentationModel<PollOptionsResponse> {

    @Getter
    private final List<PollOption> optionList;

    public PollOptionsResponse(List<PollOption> optionList) {
        this.optionList = optionList;
    }

}
