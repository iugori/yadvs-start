package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.model.rest.ctx.RestContext;
import ro.iugori.yadvs.model.rest.shared.Vote;
import ro.iugori.yadvs.service.VoteService;
import ro.iugori.yadvs.util.rest.RestApi;

@Tag(name = "votes", description = "The vote casting API")
@RestController
@RequestMapping(path = RestApi.URI.Votes.ROOT)
public class VoteResource {


    private final VoteService voteService;

    public VoteResource(VoteService voteService) {
        this.voteService = voteService;
    }

    @Operation(summary = "Cast vote", tags = {"votes"})
    @PostMapping
    public ResponseEntity<?> postVote(@Parameter(hidden = true) RestContext restCtx
            , @Check @RequestBody Vote vote) {
        voteService.addVote(restCtx, vote);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

}
