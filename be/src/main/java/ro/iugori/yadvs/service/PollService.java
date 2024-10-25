package ro.iugori.yadvs.service;

import jakarta.validation.Validator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import ro.iugori.yadvs.delegate.rest.ErrorResponseBuilder;
import ro.iugori.yadvs.model.criteria.QueryCriteria;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.domain.PollStatus;
import ro.iugori.yadvs.model.entity.PollEntity;
import ro.iugori.yadvs.model.error.CheckException;
import ro.iugori.yadvs.model.error.ErrorCode;
import ro.iugori.yadvs.model.error.ErrorModel;
import ro.iugori.yadvs.model.error.TargetType;
import ro.iugori.yadvs.model.rest.shared.Poll;
import ro.iugori.yadvs.repository.PollRepository;
import ro.iugori.yadvs.repository.PollRepositoryCustom;
import ro.iugori.yadvs.util.mapping.PollMapper;

import java.util.List;
import java.util.Optional;


@Service
@Slf4j
public class PollService {

    private final Validator validator;
    private final PollRepository pollRepository;
    private final PollRepositoryCustom pollRepositoryCustom;

    public PollService(Validator validator, PollRepository pollRepository, PollRepositoryCustom pollRepositoryCustom) {
        this.validator = validator;
        this.pollRepository = pollRepository;
        this.pollRepositoryCustom = pollRepositoryCustom;
    }

    public PollEntity create(CallContext callCtx, Poll dto) {
        checkNameIsUnique(callCtx, dto.getName());
        var entity = new PollEntity();
        PollMapper.putDto2Entity(dto, entity);
        entity.setStatus(PollStatus.DRAFT);
        return pollRepository.saveAndFlush(entity);
    }

    public Optional<PollEntity> put(CallContext callCtx, Poll dto) {
        return update(callCtx, dto, true);
    }

    public Optional<PollEntity> patch(CallContext callCtx, Poll dto) {
        return update(callCtx, dto, false);
    }

    private Optional<PollEntity> update(CallContext callCtx, Poll dto, boolean usePut) {
        var optEntity = pollRepository.findById(dto.getId());
        if (optEntity.isEmpty()) {
            return optEntity;
        }

        var entity = optEntity.get();
        var prevStatus = entity.getStatus();

        if (dto.getName() != null && !dto.getName().equals(entity.getName())) {
            checkNameIsUnique(callCtx, dto.getName());
        }

        if (usePut) {
            PollMapper.putDto2Entity(dto, entity);
        } else {
            PollMapper.patchDto2Entity(dto, entity);
            dto = PollMapper.dtoFrom(entity);
            var validationResult = validator.validate(dto);
            if (!validationResult.isEmpty()) {
                var errors = validationResult.stream().map(ErrorResponseBuilder::errorModelOf).toArray(ErrorModel[]::new);
                throw new CheckException(errors);
            }
        }

        entity.setStatus(prevStatus);
        entity = pollRepository.saveAndFlush(entity);
        return Optional.of(entity);
    }

    public Object delete(CallContext callCtx, long id) {
        var optEntity = pollRepository.findById(id);
        if (optEntity.isEmpty()) {
            return false;
        }

        var entity = optEntity.get();
        if (PollStatus.DRAFT.equals(entity.getStatus())) {
            // TODO: also delete the options - or enable cascade deletion
            pollRepository.delete(entity);
            return true;
        }

        var error = new ErrorModel();
        error.setCode(ErrorCode.NOT_ALLOWED);
        error.setMessage(String.format("Polls with status `%s' cannot be deleted (only archived)", entity.getStatus()));
        error.setTarget(TargetType.FIELD, "status");
        throw new CheckException(error);
    }

    public Optional<PollEntity> findById(long id) {
        return pollRepository.findById(id);
    }

    public List<PollEntity> find(CallContext callCtx, QueryCriteria qc) {
        if (qc == null || qc.isEmpty()) {
            return pollRepository.findAll();
        }
        return pollRepositoryCustom.findByCriteria(callCtx, qc);
    }

    public Pair<List<PollEntity>, Long> findAndCount(CallContext callCtx, QueryCriteria qc) {
        if (qc == null || qc.isEmpty()) {
            var records = pollRepository.findAll();
            return Pair.of(records, (long) records.size());
        }
        return pollRepositoryCustom.findByCriteriaAndCountTotal(callCtx, qc);
    }

    private void checkNameIsUnique(CallContext callCtx, String name) {
        var optEntity = pollRepository.findByName(name);
        if (optEntity.isPresent()) {
            log.error("{} Poll name already exists: `{}'.", callCtx.getLogRef(), name);
            var error = new ErrorModel();
            error.setCode(ErrorCode.RESOURCE_CONFLICT);
            error.setMessage("Poll.name must be unique");
            error.setTarget(TargetType.FIELD, "name");
            throw new CheckException(error);
        }
    }

}
