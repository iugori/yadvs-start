package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.*;
import ro.iugori.yadvs.model.domain.PollAction;

import java.time.LocalDateTime;

@RequiredArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@Entity
@Table(name = "poll_history")
public class PollHistoryEntity {

    @Id
    @GeneratedValue
    private Long id;

    @ManyToOne
    @JoinColumn(name = "poll_id", nullable = false)
    private PollEntity poll;


    @Column(name = "action", nullable = false, length = 11)
    @Enumerated(EnumType.STRING)
    private PollAction action;

    @Column(name = "action_utc")
    private LocalDateTime actionTime;

}
