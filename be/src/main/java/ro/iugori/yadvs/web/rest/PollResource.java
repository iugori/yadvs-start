package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.delegate.rest.QueryCriteriaBuilder;
import ro.iugori.yadvs.model.rest.GetPollsResponse;
import ro.iugori.yadvs.model.rest.Poll;
import ro.iugori.yadvs.model.rest.RestContext;
import ro.iugori.yadvs.service.PollService;
import ro.iugori.yadvs.util.mapping.PollMapper;
import ro.iugori.yadvs.util.rest.RestApi;

import java.util.List;
import java.util.stream.Collectors;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;

@Tag(name = "polls", description = "The polls management API")
@RestController
@RequestMapping(path = RestApi.URI.Polls.ROOT)
public class PollResource {

    private final PollService pollService;

    public PollResource(PollService pollService) {
        this.pollService = pollService;
    }

    @Operation(summary = "Create poll", tags = {"polls"})
    @PostMapping
    public ResponseEntity<?> postPoll(@Parameter(hidden = true) RestContext restCtx
            , @Check @RequestBody Poll poll) {
        var prefer = StringUtils.trimToEmpty(restCtx.getRequest().getHeader(RestApi.Header.PREFER));
        var entity = pollService.create(restCtx, poll);
        var headers = new LinkedMultiValueMap<String, String>();
        headers.put(HttpHeaders.LOCATION, List.of(restCtx.getRequest().getRequestURI() + "/" + entity.getId()));
        if (prefer.equalsIgnoreCase(RestApi.Header.Value.RETURN_MINIMAL)) {
            headers.put(RestApi.Header.PREFERENCE_APPLIED, List.of(prefer));
            return new ResponseEntity<>(headers, HttpStatus.NO_CONTENT);
        }
        if (prefer.equalsIgnoreCase(RestApi.Header.Value.RETURN_REPRESENTATION)) {
            headers.put(RestApi.Header.PREFERENCE_APPLIED, List.of(prefer));
        }
        return new ResponseEntity<>(PollMapper.dtoFrom(entity), headers, HttpStatus.CREATED);
    }

    @Operation(summary = "Replace poll", description = "The `status' input field is ignored.", tags = {"polls"})
    @PutMapping("/{id}")
    public ResponseEntity<?> putPoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id
            , @Check @RequestBody Poll poll) {
        poll.setId(id);
        poll.setStatus(null);
        var optPoll = pollService.put(restCtx, poll);
        return optPoll.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NOT_FOUND)
                : new ResponseEntity<>(optPoll.get(), HttpStatus.OK);
    }

    @Operation(summary = "Update poll", description = "The `status' input field is ignored.", tags = {"polls"})
    @PatchMapping(value = "/{id}", consumes = {MediaType.APPLICATION_JSON_VALUE, RestApi.MediaType.APPLICATION_MERGE_PATCH_JSON_VALUE})
    public ResponseEntity<?> patchPoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id
            , @RequestBody Poll poll) {
        poll.setId(id);
        poll.setStatus(null);
        var optPoll = pollService.patch(restCtx, poll);
        return optPoll.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NOT_FOUND)
                : new ResponseEntity<>(optPoll.get(), HttpStatus.OK);
    }

    @Operation(summary = "Activate poll", description = "Transitions the poll to `ACTIVE' state.", tags = {"polls"})
    @PatchMapping(value = "/{id}" + RestApi.URI.Polls.ACTIVATE)
    public ResponseEntity<?> activatePoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id) {
        throw new NotImplementedException("Not implemented yet.");
    }

    @Operation(summary = "Suspend poll", description = "Transitions the poll to `SUSPENDED' state.", tags = {"polls"})
    @PatchMapping(value = "/{id}" + RestApi.URI.Polls.SUSPEND)
    public ResponseEntity<?> suspendPoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id) {
        throw new NotImplementedException("Not implemented yet.");
    }

    @Operation(summary = "Close poll", description = "Transitions the poll to `CLOSED' state.", tags = {"polls"})
    @PatchMapping(value = "/{id}" + RestApi.URI.Polls.CLOSE)
    public ResponseEntity<?> closePoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id) {
        throw new NotImplementedException("Not implemented yet.");
    }

    @Operation(summary = "Archive poll", description = "Transitions the poll to `ARCHIVED' state.", tags = {"polls"})
    @PatchMapping(value = "/{id}" + RestApi.URI.Polls.ARCHIVE)
    public ResponseEntity<?> archivePoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id) {
        throw new NotImplementedException("Not implemented yet.");
    }

    @Operation(summary = "Delete poll", tags = {"polls"})
    @DeleteMapping("/{id}")
    public ResponseEntity<?> deletePoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id) {
        var delResult = pollService.delete(restCtx, id);
        if (Boolean.TRUE.equals(delResult)) {
            return new ResponseEntity<>(HttpStatus.NO_CONTENT);
        }
        if (Boolean.FALSE.equals(delResult)) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        throw new NotImplementedException("Archiving via delete is not supported.");
    }

    @Operation(summary = "Retrieve poll", tags = {"polls"}, parameters = {
            @Parameter(in = ParameterIn.HEADER,
                    name = RestApi.Header.ACCEPT_LINKS,
                    description = "If the value contains `" + RestApi.Header.Value.ACCEPT_LINKS_HATEOAS + "' then the response body will contain the Hypermedia as the engine of application state links.")
    })
    @GetMapping("/{id}")
    public ResponseEntity<?> getPoll(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id) {
        var optPoll = pollService.findById(id);
        if (optPoll.isEmpty()) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        var poll = PollMapper.dtoFrom(optPoll.get());
        if (restCtx.isFillHATEOASLinks()) {
            poll.add(linkTo(PollResource.class).slash(id).withSelfRel());
        }
        return new ResponseEntity<>(poll, HttpStatus.OK);
    }

    @Operation(summary = "Retrieve a collection of polls", tags = {"polls"}, parameters = {
            @Parameter(in = ParameterIn.HEADER,
                    name = RestApi.Header.ACCEPT_LINKS,
                    description = "If the value contains `" + RestApi.Header.Value.ACCEPT_LINKS_HATEOAS + "' then the response body will contain the Hypermedia as the engine of application state links.")
    })
    @GetMapping
    public ResponseEntity<GetPollsResponse> getPolls(@Parameter(hidden = true) RestContext restCtx) {
        var qc = QueryCriteriaBuilder.of(restCtx);
        var records = pollService.findAndCount(restCtx, qc);

        var headers = new LinkedMultiValueMap<String, String>();
        var recordCount = records.getSecond();
        headers.put(RestApi.Header.X_TOTAL_COUNT, List.of(String.valueOf(recordCount)));
        if (qc.limit() != null) {
            var pageCount = recordCount / qc.limit();
            if (recordCount % qc.limit() > 0) {
                pageCount++;
            }
            headers.put(RestApi.Header.X_TOTAL_PAGES, List.of(String.valueOf(pageCount)));
        }

        if (records.getFirst().isEmpty()) {
            return new ResponseEntity<>(headers, HttpStatus.NO_CONTENT);
        }

        var dtoList = records.getFirst().stream()
                .map(entity -> {
                    var dto = PollMapper.dtoFrom(entity);
                    if (restCtx.isFillHATEOASLinks()) {
                        dto.add(linkTo(PollResource.class).slash(dto.getId()).withSelfRel());
                    }
                    return dto;
                })
                .collect(Collectors.toList());
        return new ResponseEntity<>(new GetPollsResponse(dtoList), headers, HttpStatus.OK);
    }

}
