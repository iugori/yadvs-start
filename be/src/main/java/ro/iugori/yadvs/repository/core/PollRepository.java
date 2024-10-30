package ro.iugori.yadvs.repository.core;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ro.iugori.yadvs.model.entity.PollEntity;

import java.util.Optional;

@Repository
public interface PollRepository extends JpaRepository<PollEntity, Long> {

    Optional<PollEntity> findByName(String name);

}
