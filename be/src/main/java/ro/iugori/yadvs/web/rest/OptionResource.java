package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.NotImplementedException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.model.rest.RestContext;
import ro.iugori.yadvs.model.rest.Option;
import ro.iugori.yadvs.util.rest.RestApi;

import java.util.List;

@Tag(name = "options", description = "The poll options management API")
@RestController
@RequestMapping(path = RestApi.URI.Polls.ROOT + "/{id}" + RestApi.URI.Options.ID)
public class OptionResource {

    @Operation(summary = "Get the poll options", tags = {"options"})
    @GetMapping
    public ResponseEntity<List<Option>> getOptions(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long pollId) {
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Operation(summary = "Set the poll options", tags = {"options"})
    @PutMapping
    public ResponseEntity<List<Option>> putOptions(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long pollId
            , @Check @RequestBody List<Option> options) {
        throw new NotImplementedException("Not implemented yet.");
    }

}
