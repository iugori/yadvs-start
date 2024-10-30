package ro.iugori.yadvs.service;

import jakarta.persistence.EntityNotFoundException;
import jakarta.validation.Validator;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.domain.PollAction;
import ro.iugori.yadvs.model.domain.PollStatus;
import ro.iugori.yadvs.model.entity.PollEntity;
import ro.iugori.yadvs.model.entity.PollHistoryEntity;
import ro.iugori.yadvs.model.entity.PollResultEntity;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.rest.shared.Poll;
import ro.iugori.yadvs.repository.core.PollHistoryRepository;
import ro.iugori.yadvs.repository.core.PollRepository;
import ro.iugori.yadvs.repository.core.PollRepositoryCustom;
import ro.iugori.yadvs.repository.core.PollResultRepository;
import ro.iugori.yadvs.util.mapping.PollMapper;
import ro.iugori.yadvs.util.time.TimeUtil;

import java.util.List;
import java.util.Optional;


@Service
@Slf4j
public class PollService {

    private final Validator validator;
    private final PollRepository pollRepository;
    private final PollHistoryRepository pollHistoryRepository;
    private final PollRepositoryCustom pollRepositoryCustom;
    private final PollResultRepository pollResultRepository;

    public PollService(Validator validator
            , PollRepository pollRepository
            , PollHistoryRepository pollHistoryRepository
            , PollRepositoryCustom pollRepositoryCustom
            , PollResultRepository pollResultRepository) {
        this.validator = validator;
        this.pollRepository = pollRepository;
        this.pollHistoryRepository = pollHistoryRepository;
        this.pollRepositoryCustom = pollRepositoryCustom;
        this.pollResultRepository = pollResultRepository;
    }

    public static void checkPollIsEditable(CallContext callCtx, PollEntity entity) {
        if (!PollStatus.DRAFT.equals(entity.getStatus())) {
            log.error("{} Poll `{}' with status `{}' cannot be edited.", callCtx.getLogRef(), entity.getId(), entity.getStatus());
            var error = new ErrorModel();
            error.setCode(ErrorCode.RESOURCE_CONFLICT);
            error.setMessage("Poll cannot be edited");
            error.setTarget(TargetType.FIELD, "status");
            throw new CheckException(error);
        }
    }

    public Optional<PollEntity> findById(long id) {
        return pollRepository.findById(id);
    }

    public Pair<List<PollEntity>, Long> findAndCount(CallContext callCtx, QueryCriteria qc) {
        if (qc == null || qc.isEmpty()) {
            var records = pollRepository.findAll();
            return Pair.of(records, (long) records.size());
        }
        return pollRepositoryCustom.findByCriteriaAndCountTotal(callCtx, qc);
    }

    @Transactional
    public PollEntity create(CallContext callCtx, Poll dto) {
        checkNameIsUnique(callCtx, null, dto.getName());

        var pollEntity = new PollEntity();
        PollMapper.putDto2Entity(dto, pollEntity);
        pollEntity.setStatus(PollStatus.DRAFT);
        pollEntity = pollRepository.saveAndFlush(pollEntity);

        var historyEntity = new PollHistoryEntity();
        historyEntity.setPoll(pollEntity);
        historyEntity.setAction(PollAction.DRAFTED);
        historyEntity.setActionTime(TimeUtil.nowUTC());
        pollHistoryRepository.saveAndFlush(historyEntity);

        return pollEntity;
    }

    @Transactional
    public Optional<PollEntity> put(CallContext callCtx, Poll dto) {
        return update(callCtx, dto, true);
    }

    @Transactional
    public Optional<PollEntity> patch(CallContext callCtx, Poll dto) {
        return update(callCtx, dto, false);
    }


    @Transactional
    public Object delete(CallContext callCtx, long id) {
        var optEntity = pollRepository.findById(id);
        if (optEntity.isEmpty()) {
            return false;
        }

        var entity = optEntity.get();
        checkPollIsEditable(callCtx, entity);

        pollRepository.delete(entity);
        pollRepository.flush();
        return true;
    }

    @Transactional
    public PollEntity putStatus(CallContext callCtx, long id, PollStatus nextStatus) {
        var pollEntity = pollRepository.findById(id).orElseThrow(() -> {
            log.error("{} Poll `{}' cannot be updated because it does not exist.", callCtx.getLogRef(), id);
            return new EntityNotFoundException(String.format("Poll `%s'", id));
        });
        checkPollStatusTransition(callCtx, id, pollEntity.getStatus(), nextStatus);

        if (PollStatus.ACTIVE.equals(nextStatus)) {
            if (CollectionUtils.isEmpty(pollEntity.getOptions())) {
                log.error("{} Poll `{}' cannot be activated without any option.", callCtx.getLogRef(), id);
                var error = new ErrorModel();
                error.setCode(ErrorCode.RESOURCE_CONFLICT);
                error.setMessage("Polls with 0 options cannot be activated");
                error.setTarget(TargetType.FIELD, "options");
                throw new CheckException(error);
            }

            if (pollEntity.getHistory().stream()
                    .noneMatch(historyEntry -> historyEntry.getAction().equals(PollAction.ACTIVATED))) {
                pollEntity.getOptions().forEach(optionEntity -> {
                    var resultEntity = new PollResultEntity();
                    resultEntity.setPollId(id);
                    resultEntity.setOptionId(optionEntity.getId());
                    resultEntity.setVoteCount(0);
                    pollResultRepository.save(resultEntity);
                });
                pollResultRepository.flush();
            }
        }

        pollEntity.setStatus(nextStatus);
        pollEntity = pollRepository.saveAndFlush(pollEntity);

        var historyEntity = new PollHistoryEntity();
        historyEntity.setPoll(pollEntity);
        historyEntity.setAction(PollAction.forStatus(nextStatus));
        historyEntity.setActionTime(TimeUtil.nowUTC());
        pollHistoryRepository.saveAndFlush(historyEntity);

        return pollEntity;
    }

    private Optional<PollEntity> update(CallContext callCtx, Poll dto, boolean usePut) {
        var entity = pollRepository.findById(dto.getId());
        if (entity.isEmpty()) {
            return entity;
        }

        var pollEntity = entity.get();
        checkPollIsEditable(callCtx, pollEntity);

        var prevStatus = pollEntity.getStatus();

        if (dto.getName() != null && !dto.getName().equals(pollEntity.getName())) {
            checkNameIsUnique(callCtx, pollEntity.getId(), dto.getName());
        }

        if (usePut) {
            PollMapper.putDto2Entity(dto, pollEntity);
        } else {
            PollMapper.patchDto2Entity(dto, pollEntity);
            dto = PollMapper.dtoFrom(pollEntity);
            var validationResult = validator.validate(dto);
            if (!validationResult.isEmpty()) {
                var errors = validationResult.stream().map(ErrorModel::of).toArray(ErrorModel[]::new);
                throw new CheckException(errors);
            }
        }

        pollEntity.setStatus(prevStatus);
        pollEntity = pollRepository.saveAndFlush(pollEntity);
        return Optional.of(pollEntity);
    }

    private void checkNameIsUnique(CallContext callCtx, Long pollId, String name) {
        var entity = pollRepository.findByName(name);
        if (entity.isPresent()) {
            if (pollId == null) {
                log.error("{} Poll cannot be created. Name `{}' already exists.", callCtx.getLogRef(), name);
            } else {
                log.error("{} Poll `{}' cannot be updated. Name `{}' already exists.", callCtx.getLogRef(), pollId, name);
            }
            var error = new ErrorModel();
            error.setCode(ErrorCode.RESOURCE_CONFLICT);
            error.setMessage("Poll.name must be unique");
            error.setTarget(TargetType.FIELD, "name");
            throw new CheckException(error);
        }
    }

    private void checkPollStatusTransition(CallContext callCtx, long id, PollStatus fromStatus, PollStatus toStatus) {
        if (fromStatus.equals(toStatus)) {
            log.error("{} Poll `{}' status transition cannot take place. It is already in `{}' status.", callCtx.getLogRef(), id, fromStatus);
            var error = new ErrorModel();
            error.setCode(ErrorCode.RESOURCE_CONFLICT);
            error.setMessage("Void poll status transition.");
            error.setTarget(TargetType.FIELD, "status");
            throw new CheckException(error);
        }

        switch (fromStatus) {
            case DRAFT -> {
                if (toStatus == PollStatus.ACTIVE) {
                    return;
                }
            }
            case ACTIVE -> {
                if (toStatus == PollStatus.SUSPENDED || toStatus == PollStatus.CLOSED) {
                    return;
                }
            }
            case SUSPENDED -> {
                if (toStatus == PollStatus.ACTIVE || toStatus == PollStatus.CLOSED) {
                    return;
                }
            }
            case CLOSED -> {
                if (toStatus == PollStatus.ARCHIVED) {
                    return;
                }
            }
        }

        log.error("{} Poll `{}' cannot transition from `{}' to `{}'.", callCtx.getLogRef(), id, fromStatus, toStatus);
        var error = new ErrorModel();
        error.setCode(ErrorCode.RESOURCE_CONFLICT);
        error.setMessage("Invalid poll status transition.");
        error.setTarget(TargetType.FIELD, "status");
        throw new CheckException(error);
    }

}
