package ro.yugori.yadvs.api.dto;

import lombok.*;
import ro.yugori.yadvs.api.model.PollStatus;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
public class Poll {

    private Long id;
    private String name;
    private String description;
    private PollStatus status;
    private Boolean multiOption;
    private LocalDateTime start;
    private LocalDateTime end;

}
