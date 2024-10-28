package ro.iugori.yadvs.model.entity;

import lombok.*;

import java.io.Serializable;

@RequiredArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
public class PollResultKey implements Serializable {

    private Long pollId;
    private Long optionId;

}
