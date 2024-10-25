package ro.iugori.yadvs.model.rest.shared;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import org.springframework.hateoas.RepresentationModel;
import ro.iugori.yadvs.model.domain.PollStatus;
import ro.iugori.yadvs.model.entity.PollEntity;
import ro.iugori.yadvs.util.validation.XFieldComparison;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
@XFieldComparison(field1 = "start", rel = "lt", field2 = "end")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Poll extends RepresentationModel<Poll> {

    private Long id;

    @NotNull
    @NotEmpty
    @Size(max = PollEntity.NAME_LENGTH)
    private String name;

    @NotNull
    @NotEmpty
    @Size(max = PollEntity.DESCRIPTION_LENGTH)
    private String description;

    private PollStatus status;

    private Boolean multiOption;

    private LocalDateTime start;

    private LocalDateTime end;

}