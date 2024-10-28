package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;

@RequiredArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@Entity
@Table(name = "vote")
public class VoteEntity {

    @Id
    @GeneratedValue
    private Long id;

    @ManyToOne
    @JoinColumn(name = "option_id", nullable = false)
    private PollOptionEntity option;

    @Column(name = "cast_on_utc")
    private LocalDateTime castOn;

}
