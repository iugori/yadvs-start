package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.callcontext.InjectCallContext;
import ro.iugori.yadvs.aop.validation.Check;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.service.PollService;
import ro.iugori.yadvs.util.mapping.PollMapper;
import ro.iugori.yadvs.web.URIs;

import java.util.List;

@RestController
@RequestMapping(path = URIs.PATH_POLLS)
@InjectCallContext
public class PollResources {

    private final PollService pollService;

    public PollResources(PollService pollService) {
        this.pollService = pollService;
    }

    @PostMapping
    public ResponseEntity<?> postPoll(@Parameter(hidden = true) RestContext restCtx, @Check @RequestBody Poll poll) {
        var entity = pollService.createPoll(poll);
        return new ResponseEntity<>(PollMapper.dtoFrom(entity), HttpStatus.CREATED);
    }

    @PutMapping
    public ResponseEntity<?> putPoll(@Parameter(hidden = true) RestContext restCtx, @RequestBody Poll poll) {
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    @PatchMapping
    public ResponseEntity<?> patchPoll(@Parameter(hidden = true) RestContext restCtx, @RequestBody Poll poll) {
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> deletePoll(@Parameter(hidden = true) RestContext restCtx, @PathVariable("id") long id) {
        pollService.deletePoll(id);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @GetMapping
    public ResponseEntity<List<Poll>> getPolls(@Parameter(hidden = true) RestContext restCtx) {
        var polls = pollService.findPolls().orElse(List.of());
        return polls.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NO_CONTENT)
                : new ResponseEntity<>(polls, HttpStatus.OK);
    }

}
