package ro.iugori.yadvs.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.SequenceGenerator;


@Entity
public class PollResult {

    @Id
    @Column(nullable = false, updatable = false)
    @SequenceGenerator(
            name = "primary_sequence",
            sequenceName = "primary_sequence",
            allocationSize = 1,
            initialValue = 10000
    )
    @GeneratedValue(
            strategy = GenerationType.SEQUENCE,
            generator = "primary_sequence"
    )
    private Long id;

    @Column(nullable = false)
    private Long pollId;

    @Column(nullable = false)
    private Long optionId;

    @Column(nullable = false)
    private Integer voteCount;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "poll_id", nullable = false)
    private Poll poll;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "option_id", nullable = false, unique = true)
    private PollOption option;

    public Long getId() {
        return id;
    }

    public void setId(final Long id) {
        this.id = id;
    }

    public Long getPollId() {
        return pollId;
    }

    public void setPollId(final Long pollId) {
        this.pollId = pollId;
    }

    public Long getOptionId() {
        return optionId;
    }

    public void setOptionId(final Long optionId) {
        this.optionId = optionId;
    }

    public Integer getVoteCount() {
        return voteCount;
    }

    public void setVoteCount(final Integer voteCount) {
        this.voteCount = voteCount;
    }

    public Poll getPoll() {
        return poll;
    }

    public void setPoll(final Poll poll) {
        this.poll = poll;
    }

    public PollOption getOption() {
        return option;
    }

    public void setOption(final PollOption option) {
        this.option = option;
    }

}
