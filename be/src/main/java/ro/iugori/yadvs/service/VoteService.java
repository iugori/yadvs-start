package ro.iugori.yadvs.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.domain.PollStatus;
import ro.iugori.yadvs.model.entity.VoteEntity;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.rest.shared.Vote;
import ro.iugori.yadvs.repository.PollOptionRepository;
import ro.iugori.yadvs.repository.VoteRepository;
import ro.iugori.yadvs.util.time.TimeUtil;

@Service
@Slf4j
public class VoteService {

    private final VoteRepository voteRepository;
    private final PollOptionRepository pollOptionRepository;

    public VoteService(VoteRepository voteRepository, PollOptionRepository pollOptionRepository) {
        this.voteRepository = voteRepository;
        this.pollOptionRepository = pollOptionRepository;
    }

    @Transactional
    public void addVote(CallContext callCtx, Vote vote) {
        var optionEntity = pollOptionRepository.findById(vote.getOptionId()).orElseThrow(() -> {
            log.error("{} Cannot add vote to non-existent option `{}'.", callCtx.getLogRef(), vote.getOptionId());
            var error = new ErrorModel();
            error.setCode(ErrorCode.RESOURCE_CONFLICT);
            error.setMessage("Vote cannot be cast (poll option does not exist).");
            error.setTarget(TargetType.FIELD, "optionId");
            return new CheckException(error);
        });

        if (optionEntity.getPoll().getStatus() != PollStatus.ACTIVE) {
            log.error("{} Cannot add vote to non `{}`' poll `{}'.", callCtx.getLogRef(), PollStatus.ACTIVE, optionEntity.getPoll().getId());
            var error = new ErrorModel();
            error.setCode(ErrorCode.RESOURCE_CONFLICT);
            error.setMessage(String.format("Vote cannot be cast (poll is not `%s').", PollStatus.ACTIVE));
            error.setTarget(TargetType.FIELD, "status");
            throw new CheckException(error);
        }

        var voteEntity = new VoteEntity();
        voteEntity.setOption(optionEntity);
        voteEntity.setCastOn(TimeUtil.nowUTC());
        voteRepository.saveAndFlush(voteEntity);
    }

}