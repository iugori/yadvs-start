package ro.iugori.yadvs.repos;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.iugori.yadvs.domain.PollOption;


public interface PollOptionRepository extends JpaRepository<PollOption, Long> {
}
