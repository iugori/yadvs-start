package ro.iugori.yadvs.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ro.iugori.yadvs.model.entity.PollHistoryEntity;

@Repository
public interface PollHistoryRepository extends JpaRepository<PollHistoryEntity, Long> {
}
