package ro.iugori.yadvs.model.rest.sturctured;

import lombok.Getter;
import org.springframework.hateoas.RepresentationModel;
import ro.iugori.yadvs.model.rest.shared.Poll;

import java.util.List;

public class GetPollsResponse extends RepresentationModel<GetPollsResponse> {

    @Getter
    private final List<Poll> pollList;

    public GetPollsResponse(List<Poll> pollList) {
        this.pollList = pollList;
    }

}
