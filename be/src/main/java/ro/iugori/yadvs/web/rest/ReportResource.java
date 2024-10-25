package ro.iugori.yadvs.web.rest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.NotImplementedException;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ro.iugori.yadvs.aop.rest.Check;
import ro.iugori.yadvs.model.rest.shared.PollReportSpec;
import ro.iugori.yadvs.util.rest.RestApi;

@Tag(name = "reporting", description = "The report generation API")
@RestController
@RequestMapping(path = RestApi.URI.Reports.ROOT)
public class ReportResource {

    @Operation(summary = "Get poll report", tags = {"reporting"})
    @GetMapping(value = RestApi.URI.Reports.POLL + "/{id}", produces = {MediaType.IMAGE_PNG_VALUE})
    public ResponseEntity<?> getPollReport(@PathVariable("id") long id
            , @Check @RequestBody PollReportSpec spec) {
        throw new NotImplementedException("Not implemented yet.");
    }

}
