package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Entity
@Table(name = "vote")
public class  VoteEntity {

    @Id
    @GeneratedValue
    private Long id;

    @ManyToOne
    @JoinColumn(name = "option_id", nullable = false)
    private PollOptionEntity option;

    @Column(name = "cast_on_utc")
    private LocalDateTime castOn;

}
