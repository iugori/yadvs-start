package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.springframework.hateoas.CollectionModel;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.ctx.RestContext;
import ro.iugori.yadvs.model.error.YadvsRestException;
import ro.iugori.yadvs.model.rest.Poll;
import ro.iugori.yadvs.service.PollService;
import ro.iugori.yadvs.util.mapping.PollMapper;
import ro.iugori.yadvs.web.RestApi;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;

@RestController
@RequestMapping(path = RestApi.URI.Polls.ROOT)
public class PollResources {

    private final PollService pollService;

    public PollResources(PollService pollService) {
        this.pollService = pollService;
    }

    @PostMapping
    public ResponseEntity<?> postPoll(@Parameter(hidden = true) RestContext restCtx, @Check @RequestBody Poll poll) {
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

    @PutMapping("/{id}")
    public ResponseEntity<?> putPoll(@Parameter(hidden = true) RestContext restCtx, @PathVariable("id") long id, @Check @RequestBody Poll poll) {
        poll.setId(id);
        var optPoll = pollService.put(restCtx, poll);
        return optPoll.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NOT_FOUND)
                : new ResponseEntity<>(optPoll.get(), HttpStatus.OK);
    }

    @PatchMapping("/{id}")
    public ResponseEntity<?> patchPoll(@Parameter(hidden = true) RestContext restCtx, @PathVariable("id") long id, @RequestBody Poll poll) {
        poll.setId(id);
        var optPoll = pollService.patch(restCtx, poll);
        return optPoll.isEmpty()
                ? new ResponseEntity<>(HttpStatus.NOT_FOUND)
                : new ResponseEntity<>(optPoll.get(), HttpStatus.OK);
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
        throw new YadvsRestException(restCtx, "Archiving via delete not yet supported");
    }

    @GetMapping("/{id}")
    public ResponseEntity<?> getPoll(@PathVariable("id") long id) {
        var optPoll = pollService.findById(id);
        if (optPoll.isEmpty()) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        var poll = PollMapper.dtoFrom(optPoll.get());
        poll.add(linkTo(PollResources.class).slash(id).withSelfRel());
        return new ResponseEntity<>(poll, HttpStatus.OK);
    }

    @GetMapping
    public ResponseEntity<CollectionModel<Poll>> getPolls(@Parameter(hidden = true) RestContext restCtx
            , @RequestParam(RestApi.Param.FIELDS) Optional<String> fields
            , @RequestParam(RestApi.Param.SORT) Optional<String> sorting
            , @RequestParam(RestApi.Param.PAGE_NO) Optional<String> pageNo
            , @RequestParam(RestApi.Param.PAGE_SIZE) Optional<String> pageSize
    ) {
        var qcBuilder = QueryCriteria.builder()
                .select(fields.orElse(null))
                .orderBy(sorting.orElse(null))
                .page(pageNo.orElse(null), pageSize.orElse(null));

        var queryParams = restCtx.getRequest().getParameterMap();
        for (var entry : queryParams.entrySet()) {
            var key = entry.getKey();
            if (!key.startsWith(RestApi.RESERVED_PARAM)) {
                var value = entry.getValue();
                if (value != null) {
                    qcBuilder.where(key, value.length == 1 ? value[0] : value);
                }
            }
        }

        var records = pollService.findAndCount(restCtx, qcBuilder.build());

        var headers = new LinkedMultiValueMap<String, String>();
        headers.put(RestApi.Header.X_TOTAL_COUNT, List.of(String.valueOf(records.getSecond())));

        if (records.getFirst().isEmpty()) {
            return new ResponseEntity<>(headers, HttpStatus.NO_CONTENT);
        }

        var dtoList = records.getFirst().stream()
                .map(entity -> {
                    var dto = PollMapper.dtoFrom(entity);
                    dto.add(linkTo(PollResources.class).slash(dto.getId()).withSelfRel());
                    return dto;
                })
                .collect(Collectors.toList());
        return new ResponseEntity<>(CollectionModel.of(dtoList), headers, HttpStatus.OK);
    }

}
