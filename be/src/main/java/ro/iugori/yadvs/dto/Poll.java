package ro.iugori.yadvs.dto;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import ro.iugori.yadvs.model.domain.PollStatus;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
public class Poll {

    private Long id;

    @NotNull
    @NotEmpty
    @Size(max = 200)
    private String name;

    @NotNull
    @NotEmpty
    @Size(max = 2000)
    private String description;

    private PollStatus status;

    private boolean multiOption;

    private LocalDateTime start;

    private LocalDateTime end;

}
