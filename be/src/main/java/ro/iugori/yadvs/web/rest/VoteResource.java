package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.NotImplementedException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.model.rest.Vote;
import ro.iugori.yadvs.util.rest.RestApi;

@Tag(name = "votes", description = "The vote casting API")
@RestController
@RequestMapping(path = RestApi.URI.Votes.ROOT)
public class VoteResource {

    @Operation(summary = "Cast vote", tags = {"votes"})
    @PostMapping
    public ResponseEntity<?> castVote(@Check @RequestBody Vote vote) {
        throw new NotImplementedException("Not implemented yet.");
    }

}
