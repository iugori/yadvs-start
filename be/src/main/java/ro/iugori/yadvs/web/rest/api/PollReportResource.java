package ro.iugori.yadvs.web.rest.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ro.iugori.yadvs.delegate.reporting.PollReportGenerator;
import ro.iugori.yadvs.web.rest.model.RestContext;
import ro.iugori.yadvs.service.api.PollOptionService;
import ro.iugori.yadvs.service.api.VoteService;
import ro.iugori.yadvs.util.rest.RestApi;

import java.io.IOException;

@Tag(name = "reporting", description = "The report generation API")
@RestController
@RequestMapping(path = RestApi.URI.Polls.ROOT + "/{id}" + RestApi.URI.Reports.ID)
public class PollReportResource {

    private final PollOptionService pollOptionService;
    private final VoteService voteService;

    public PollReportResource(PollOptionService pollOptionService, VoteService voteService) {
        this.pollOptionService = pollOptionService;
        this.voteService = voteService;
    }

    @Operation(summary = "Get poll report", tags = {"reporting"})
    @GetMapping()
    public ResponseEntity<?> getPollReport(@Parameter(hidden = true) RestContext restCtx
            , @PathVariable("id") long id) throws IOException {
        var reportGenerator = new PollReportGenerator(pollOptionService.getOptions(id), voteService.getVotes(id));
        var headers = new HttpHeaders();

        var accept = StringUtils.trimToEmpty(restCtx.getRequest().getHeader(HttpHeaders.ACCEPT));
        if (MediaType.IMAGE_PNG_VALUE.equalsIgnoreCase(accept)) {
            var pngReport = reportGenerator.generatePngReport();
            headers.setContentType(MediaType.IMAGE_PNG);
            headers.setContentLength(pngReport.length);
            return new ResponseEntity<>(pngReport, headers, HttpStatus.OK);
        }

        headers.setContentType(MediaType.APPLICATION_JSON);
        return new ResponseEntity<>(reportGenerator.generateBasicReport(), headers, HttpStatus.OK);
    }


}
