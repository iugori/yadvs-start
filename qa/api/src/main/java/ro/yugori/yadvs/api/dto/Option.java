package ro.yugori.yadvs.api.dto;

import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
public class Option {

    private Long id;
    private Short index;
    private String description;

}
