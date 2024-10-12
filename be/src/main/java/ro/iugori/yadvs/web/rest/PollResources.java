package ro.iugori.yadvs.web.rest;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ro.iugori.yadvs.config.REST;
import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.service.PollService;

import java.util.List;

@RestController
@RequestMapping(path = REST.PATH_POLLS)
public class PollResources {

    private final PollService pollService;

    public PollResources(PollService pollService) {
        this.pollService = pollService;
    }

    @GetMapping
    public ResponseEntity<List<Poll>> getPolls() {
        var polls = pollService.findPolls().orElse(List.of());
        return polls.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NO_CONTENT)
                : new ResponseEntity<>(polls, HttpStatus.OK);
    }

}
