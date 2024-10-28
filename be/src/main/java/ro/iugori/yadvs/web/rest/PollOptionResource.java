package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.model.entity.PollOptionEntity;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.rest.ctx.RestContext;
import ro.iugori.yadvs.model.rest.sturctured.PollOptionsResponse;
import ro.iugori.yadvs.model.rest.sturctured.PutPollOptionsRequest;
import ro.iugori.yadvs.service.PollOptionService;
import ro.iugori.yadvs.util.mapping.PollOptionMapper;
import ro.iugori.yadvs.util.rest.RestApi;

import java.util.List;

@Tag(name = "options", description = "The poll options management API")
@RestController
@RequestMapping(path = RestApi.URI.Polls.ROOT + "/{id}" + RestApi.URI.Options.ID)
public class PollOptionResource {

    private final PollOptionService pollOptionService;

    public PollOptionResource(PollOptionService pollOptionService) {
        this.pollOptionService = pollOptionService;
    }

    @Operation(summary = "Set the poll options", tags = {"options"})
    @PutMapping
    public ResponseEntity<PollOptionsResponse> putOptions(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long pollId
            , @Check @RequestBody PutPollOptionsRequest request) {
        var options = pollOptionService.putOptions(restCtx, pollId, request.getOptionList());
        if (options.getFirst().isEmpty()) {
            return new ResponseEntity<>(HttpStatus.NO_CONTENT);
        }
        var restResponse = new PollOptionsResponse(options.getFirst().stream().map(PollOptionMapper::dtoFrom).toList());
        var restResponseStatus = options.getSecond() ? HttpStatus.CREATED : HttpStatus.OK;
        return new ResponseEntity<>(restResponse, restResponseStatus);
    }

    @Operation(summary = "Get the poll options", tags = {"options"})
    @GetMapping
    public ResponseEntity<PollOptionsResponse> getOptions(@PathVariable("id") String pollIdHint) {
        List<PollOptionEntity> optionEntities;
        if (RestApi.URI.WILDCARD.equals(pollIdHint)) {
            optionEntities = pollOptionService.getAllOptions();
        } else {
            try {
                var pollId = Long.parseLong(pollIdHint);
                optionEntities = pollOptionService.getOptions(pollId);
            } catch (NumberFormatException e) {
                throw new CheckException(ErrorModel.of(e, TargetType.PARAMETER, "/{id}/"));
            }
        }
        if (optionEntities.isEmpty()) {
            return new ResponseEntity<>(HttpStatus.NO_CONTENT);
        }
        return new ResponseEntity<>(new PollOptionsResponse(optionEntities.stream().map(PollOptionMapper::dtoFrom).toList()), HttpStatus.OK);
    }

}
