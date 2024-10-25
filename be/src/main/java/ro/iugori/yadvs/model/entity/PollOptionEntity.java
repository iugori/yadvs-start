package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.Data;

@Data
@Entity
@Table(name = "poll_option")
public class PollOptionEntity {

    public static final int DESCRIPTION_LENGTH = 2000;

    @Id
    @GeneratedValue
    private Long id;

    @ManyToOne
    @JoinColumn(name = "poll_id", nullable = false)
    private PollEntity poll;

    @Column(name = "position", nullable = false, precision = 2)
    private Short position;

    @Column(name = "description", nullable = false, length = DESCRIPTION_LENGTH)
    private String description;

}