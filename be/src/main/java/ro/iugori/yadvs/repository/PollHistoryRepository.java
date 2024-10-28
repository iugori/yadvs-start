package ro.iugori.yadvs.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.iugori.yadvs.model.entity.PollHistoryEntity;

public interface PollHistoryRepository extends JpaRepository<PollHistoryEntity, Long> {
}
