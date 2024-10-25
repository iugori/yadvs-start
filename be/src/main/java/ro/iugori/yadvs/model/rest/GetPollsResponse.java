package ro.iugori.yadvs.model.rest;

import lombok.Getter;
import org.springframework.hateoas.RepresentationModel;

import java.util.List;

public class GetPollsResponse extends RepresentationModel<GetPollsResponse> {

    @Getter
    private final List<Poll> pollList;

    public GetPollsResponse(List<Poll> pollList) {
        this.pollList = pollList;
    }

}
