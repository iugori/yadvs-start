package ro.iugori.yadvs.web.rest;

import jakarta.validation.Validator;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.callcontext.InjectCallContext;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.delegate.rest.ErrorBuilder;
import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.model.rest.ErrorResponse;
import ro.iugori.yadvs.service.PollService;
import ro.iugori.yadvs.util.mapping.PollMapper;
import ro.iugori.yadvs.web.URIs;

import java.util.List;

@RestController
@RequestMapping(path = URIs.PATH_POLLS)
@InjectCallContext
public class PollResources {

    private final Validator validator;
    private final PollService pollService;

    public PollResources(Validator validator, PollService pollService) {
        this.validator = validator;
        this.pollService = pollService;
    }

    @PostMapping
    public ResponseEntity<?> postPoll(RestContext restCtx, @RequestBody Poll poll) {
        var validResult = validator.validate(poll);
        if (!validResult.isEmpty()) {
            var errors = new ErrorResponse(restCtx.getTraceId());
            validResult.forEach(constraint -> errors.add(ErrorBuilder.of(restCtx.getRequest(), constraint)));
            return new ResponseEntity<>(errors, HttpStatus.BAD_REQUEST);
        }
        var entity = pollService.createPoll(poll);
        return new ResponseEntity<>(PollMapper.dtoFrom(entity), HttpStatus.CREATED);
    }

    @GetMapping
    public ResponseEntity<List<Poll>> getPolls(RestContext restCtx) {
        var polls = pollService.findPolls().orElse(List.of());
        return polls.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NO_CONTENT)
                : new ResponseEntity<>(polls, HttpStatus.OK);
    }

}
