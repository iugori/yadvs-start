package ro.iugori.yadvs.web.rest;

import jakarta.validation.Validator;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.config.REST;
import ro.iugori.yadvs.delegates.rest.ErrorBuilder;
import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.model.rest.ErrorResponse;
import ro.iugori.yadvs.service.PollService;

import java.util.List;

@RestController
@RequestMapping(path = REST.PATH_POLLS)
public class PollResources {

    private final Validator validator;
    private final PollService pollService;

    public PollResources(Validator validator, PollService pollService) {
        this.validator = validator;
        this.pollService = pollService;
    }

    @PostMapping
    public ResponseEntity<?> postPoll(@RequestBody Poll poll) {
        var validResult = validator.validate(poll);
        if (!validResult.isEmpty()) {
            var errors = new ErrorResponse();
            validResult.forEach(constraint -> errors.add(ErrorBuilder.of(constraint)));
            return new ResponseEntity<>(errors, HttpStatus.BAD_REQUEST);
        }
        return new ResponseEntity<>(poll, HttpStatus.CREATED);
    }

    @GetMapping
    public ResponseEntity<List<Poll>> getPolls() {
        var polls = pollService.findPolls().orElse(List.of());
        return polls.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NO_CONTENT)
                : new ResponseEntity<>(polls, HttpStatus.OK);
    }

}
