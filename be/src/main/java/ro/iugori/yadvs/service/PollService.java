package ro.iugori.yadvs.service;

import org.springframework.stereotype.Service;
import ro.iugori.yadvs.dto.Poll;

import java.util.List;
import java.util.Optional;

@Service
public class PollService {

    public Optional<List<Poll>> findPolls() {
        return Optional.empty();
    }

}
