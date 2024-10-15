package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.Data;
import ro.iugori.yadvs.model.domain.PollStatus;

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

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    private PollStatus status;

    @Column(name = "multi_option")
    private boolean multiOption;



}
