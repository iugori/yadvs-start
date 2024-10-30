package ro.iugori.yadvs.repository.api;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ro.iugori.yadvs.model.entity.VoteEntity;

@Repository
public interface VoteRepository extends JpaRepository<VoteEntity, Long> {
}
