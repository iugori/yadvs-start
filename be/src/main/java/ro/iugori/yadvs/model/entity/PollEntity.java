package ro.iugori.yadvs.model.entity;

import jakarta.persistence.*;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import ro.iugori.yadvs.model.domain.PollStatus;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@RequiredArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@Entity
@Table(uniqueConstraints = @UniqueConstraint(columnNames = "name"), name = "poll")
public class PollEntity {

    public static final int NAME_LENGTH = 200;
    public static final int DESCRIPTION_LENGTH = 2000;

    @Id
    @GeneratedValue
    private Long id;

    @Column(name = "name", nullable = false, length = NAME_LENGTH)
    private String name;

    @Column(name = "description", nullable = false, length = DESCRIPTION_LENGTH)
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

    @OneToMany(mappedBy = "poll", cascade = CascadeType.REMOVE, fetch = FetchType.LAZY)
    private List<PollOptionEntity> options = new ArrayList<>();

}
