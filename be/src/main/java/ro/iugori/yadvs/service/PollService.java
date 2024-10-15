package ro.iugori.yadvs.service;

import org.springframework.stereotype.Service;
import ro.iugori.yadvs.delegate.ctx.CallContext;
import ro.iugori.yadvs.delegate.ctx.RestContext;
import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.model.domain.PollStatus;
import ro.iugori.yadvs.model.domain.TargetType;
import ro.iugori.yadvs.model.entity.PollEntity;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.repository.PollRepository;
import ro.iugori.yadvs.util.mapping.PollMapper;

import java.util.List;
import java.util.Optional;

@Service
public class PollService {

    private final PollRepository pollRepository;

    public PollService(PollRepository pollRepository) {
        this.pollRepository = pollRepository;
    }

    public PollEntity create(CallContext callCtx, Poll poll) {
        var optEntity = pollRepository.findByName(poll.getName());
        if (optEntity.isPresent()) {
            var error = new ErrorModel();
            error.setCode(ErrorCode.VALUE_CONFLICT);
            error.setMessage("Poll `name' must be unique");
            error.setTarget(TargetType.FIELD, "name");
            throw new CheckException(callCtx, error);
        }
        var entity = new PollEntity();
        PollMapper.put(poll, entity);
        entity.setStatus(PollStatus.DRAFT);
        entity = pollRepository.saveAndFlush(entity);
        return entity;
    }

    public Object delete(CallContext callCtx, long id) {
        var optEntity = pollRepository.findById(id);
        if (optEntity.isEmpty()) {
            return false;
        }
        var entity = optEntity.get();
        if (PollStatus.DRAFT.equals(entity.getStatus())) {
            // also delete the options - or enable cascade deletion
            pollRepository.delete(entity);
            return true;
        }
        return optEntity.get();
    }

    public Optional<PollEntity> findById(RestContext restCtx, long id) {
        return pollRepository.findById(id);
    }

    public List<Poll> find() {
        return List.of();
    }

}
