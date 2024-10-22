package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.NotImplementedException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.model.ctx.RestContext;
import ro.iugori.yadvs.model.rest.Option;
import ro.iugori.yadvs.util.rest.RestApi;

@Tag(name = "Option", description = "the poll options management API")
@RestController
@RequestMapping(path = RestApi.URI.Polls.ROOT + "/{id}" + RestApi.URI.Options.ID)
public class OptionResource {

    @Operation(summary = "Add poll option", tags = {"Option"})
    @PostMapping
    public ResponseEntity<?> postOption(@Parameter(hidden = true) RestContext restCtx
            , @Check @RequestBody Option poll) {
        throw new NotImplementedException("Not implemented yet.");
    }

}
