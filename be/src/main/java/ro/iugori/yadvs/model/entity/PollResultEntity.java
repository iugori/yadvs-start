package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.*;

@RequiredArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@Entity
@Table(name = "poll_result")
@IdClass(PollResultKey.class)
public class PollResultEntity {

    @Id
    @JoinColumn(name = "poll_id")
    private Long pollId;

    @Id
    @JoinColumn(name = "option_id")
    private Long optionId;

    @Column(name = "vote_count", nullable = false, precision = 10)
    private Integer position;

}
