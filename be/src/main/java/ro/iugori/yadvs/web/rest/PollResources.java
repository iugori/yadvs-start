package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.model.error.YadvsException;
import ro.iugori.yadvs.model.rest.MoreHttpHeaders;
import ro.iugori.yadvs.service.PollService;
import ro.iugori.yadvs.util.mapping.PollMapper;
import ro.iugori.yadvs.web.URIs;

import java.util.List;

@RestController
@RequestMapping(path = URIs.PATH_POLLS)
public class PollResources {

    private final PollService pollService;

    public PollResources(PollService pollService) {
        this.pollService = pollService;
    }

    @PostMapping
    public ResponseEntity<?> postPoll(@Parameter(hidden = true) RestContext restCtx, @Check @RequestBody Poll poll) {
        var prefer = StringUtils.trimToEmpty(restCtx.getRequest().getHeader(MoreHttpHeaders.PREFER));
        var entity = pollService.create(restCtx, poll);
        var headers = new LinkedMultiValueMap<String, String>();
        headers.put(HttpHeaders.LOCATION, List.of(restCtx.getRequest().getRequestURI() + "/" + entity.getId()));
        if (prefer.equalsIgnoreCase("return=minimal")) {
            headers.put(MoreHttpHeaders.PREFERENCE_APPLIED, List.of(prefer));
            return new ResponseEntity<>(headers, HttpStatus.NO_CONTENT);
        }
        if (prefer.equalsIgnoreCase("return=representation")) {
            headers.put(MoreHttpHeaders.PREFERENCE_APPLIED, List.of(prefer));
        }
        return new ResponseEntity<>(PollMapper.dtoFrom(entity), headers, HttpStatus.CREATED);
    }

    @PutMapping("/{id}")
    public ResponseEntity<?> putPoll(@Parameter(hidden = true) RestContext restCtx, @Check @RequestBody Poll poll) {
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    @PatchMapping("/{id}")
    public ResponseEntity<?> patchPoll(@Parameter(hidden = true) RestContext restCtx, @Check @RequestBody Poll poll) {
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> deletePoll(@Parameter(hidden = true) RestContext restCtx, @PathVariable("id") long id) {
        var delResult = pollService.delete(restCtx, id);
        if (Boolean.TRUE.equals(delResult)) {
            return new ResponseEntity<>(HttpStatus.NO_CONTENT);
        }
        if (Boolean.FALSE.equals(delResult)) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        throw new YadvsException(restCtx, "Archiving via delete not yet supported");
    }

    @GetMapping("/{id}")
    public ResponseEntity<?> getPoll(@Parameter(hidden = true) RestContext restCtx, @PathVariable("id") long id) {
        var optPoll = pollService.findById(restCtx, id);
        return optPoll.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NOT_FOUND)
                : new ResponseEntity<>(optPoll.get(), HttpStatus.OK);
    }

    @GetMapping
    public ResponseEntity<List<Poll>> getPolls(@Parameter(hidden = true) RestContext restCtx) {
        var polls = pollService.find();
        return polls.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NO_CONTENT)
                : new ResponseEntity<>(polls, HttpStatus.OK);
    }

}
