package ro.iugori.yadvs.repos;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.iugori.yadvs.domain.Poll;


public interface PollRepository extends JpaRepository<Poll, Long> {
}
