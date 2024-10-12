package ro.iugori.yadvs.repos;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.iugori.yadvs.domain.PollHistory;


public interface PollHistoryRepository extends JpaRepository<PollHistory, Long> {
}
