package ro.iugori.yadvs.repos;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.iugori.yadvs.domain.PollResult;


public interface PollResultRepository extends JpaRepository<PollResult, Long> {
}
