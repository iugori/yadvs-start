package ro.iugori.yadvs.service;

import org.springframework.stereotype.Service;
import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.model.entity.PollEntity;
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

    public Optional<List<Poll>> findPolls() {
        return Optional.empty();
    }

    public PollEntity createPoll(Poll poll) {
        var entity = new PollEntity();
        PollMapper.put(poll, entity);
        entity = pollRepository.saveAndFlush(entity);
        return entity;
    }

}
