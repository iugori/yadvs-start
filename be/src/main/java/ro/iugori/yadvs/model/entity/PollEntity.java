package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.Data;
import ro.iugori.yadvs.model.domain.PollStatus;

import java.time.LocalDateTime;

@Data
@Entity
@Table(uniqueConstraints = @UniqueConstraint(columnNames = "name"), name = "poll")
public class PollEntity {

    @Id
    @GeneratedValue
    private Long id;

    @Column(name = "name", nullable = false, length = 200)
    private String name;

    @Column(name = "description", nullable = false, length = 2000)
    private String description;

    @Column(name = "status", nullable = false, length = 9)
    @Enumerated(EnumType.STRING)
    private PollStatus status;

    @Column(name = "multi_option")
    private Boolean multiOption;

    @Column(name = "start_utc")
    private LocalDateTime start;

    @Column(name = "end_utc")
    private LocalDateTime end;

}
