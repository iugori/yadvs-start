package ro.iugori.yadvs.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.entity.PollOptionEntity;
import ro.iugori.yadvs.model.rest.shared.PollOption;
import ro.iugori.yadvs.repository.PollOptionRepository;
import ro.iugori.yadvs.util.mapping.PollOptionMapper;

import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.IntStream;

@Service
@Slf4j
public class PollOptionService {

    private final PollService pollService;
    private final PollOptionRepository pollOptionRepository;

    public PollOptionService(PollService pollService, PollOptionRepository pollOptionRepository) {
        this.pollService = pollService;
        this.pollOptionRepository = pollOptionRepository;
    }

    public List<PollOptionEntity> getAllOptions() {
        return pollOptionRepository.findAll();
    }

    public List<PollOptionEntity> getOptions(long pollId) {
        pollService.findById(pollId).orElseThrow(() -> new NoSuchElementException(String.format("Cannot find poll`%d'.", pollId)));
        var options = pollOptionRepository.findByPollId(pollId);
        Collections.sort(options);
        return options;
    }

    @Transactional
    public Pair<List<PollOptionEntity>, Boolean> putOptions(CallContext callCtx, long pollId, List<PollOption> dtoList) {
        var entityList = getOptions(pollId);
        int commonLength = Math.min(entityList.size(), dtoList.size());

        var pollEntity = commonLength > 0
                ? entityList.getFirst().getPoll()
                : pollService.findById(pollId).orElseThrow();
        PollService.checkPollIsEditable(callCtx, pollEntity);

        Collections.sort(dtoList);
        IntStream.range(0, dtoList.size()).forEach(i -> dtoList.get(i).setPosition((short) (i + 1)));
        var created = false;

        IntStream.range(0, commonLength).forEach(i -> {
            var entity = entityList.get(i);
            PollOptionMapper.putDto2EntityUserInput(dtoList.get(i), entity);
            pollOptionRepository.save(entity);
        });

        if (entityList.size() > commonLength) {
            IntStream.range(commonLength, entityList.size()).forEach(i -> {
                pollOptionRepository.delete(entityList.get(commonLength));
                entityList.remove(commonLength);
            });
        }

        if (dtoList.size() > commonLength) {
            created = true;
            IntStream.range(commonLength, dtoList.size()).forEach(i -> {
                var entity = new PollOptionEntity();
                PollOptionMapper.putDto2EntityUserInput(dtoList.get(i), entity);
                entity.setPoll(pollEntity);
                entity.setPosition((short) (i + 1));
                entity = pollOptionRepository.save(entity);
                entityList.add(entity);
            });
        }

        pollOptionRepository.flush();
        return Pair.of(entityList, created);
    }


}
