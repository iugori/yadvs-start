package ro.iugori.yadvs.repository.core;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ro.iugori.yadvs.model.entity.PollOptionEntity;

import java.util.List;

@Repository
public interface PollOptionRepository extends JpaRepository<PollOptionEntity, Long> {

    List<PollOptionEntity> findByPollId(long pollId);

}
