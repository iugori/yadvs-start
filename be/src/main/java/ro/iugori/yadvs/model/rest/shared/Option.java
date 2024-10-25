package ro.iugori.yadvs.model.rest.shared;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import ro.iugori.yadvs.model.entity.PollOptionEntity;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Option {

    private Long id;

    @NotNull
    private Short position;

    @NotNull
    @NotEmpty
    @Size(max = PollOptionEntity.DESCRIPTION_LENGTH)
    private String description;

}
