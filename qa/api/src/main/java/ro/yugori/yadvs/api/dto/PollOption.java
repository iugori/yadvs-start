package ro.yugori.yadvs.api.dto;

import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
public class PollOption {

    private Long id;
    private Short position;
    private String description;

}
