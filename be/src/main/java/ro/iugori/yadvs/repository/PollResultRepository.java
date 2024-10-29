package ro.iugori.yadvs.repository;

import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import ro.iugori.yadvs.model.entity.PollResultEntity;
import ro.iugori.yadvs.model.entity.PollResultKey;

import java.util.Optional;

@Repository
public interface PollResultRepository extends JpaRepository<PollResultEntity, PollResultKey> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT r FROM PollResultEntity r WHERE r.pollId = :pollId AND r.optionId = :optionId")
    Optional<PollResultEntity> findForUpdate(@Param("pollId") Long pollId, @Param("optionId") Long optionId);

    @Modifying
    @Query("UPDATE PollResultEntity r SET r.voteCount = r.voteCount + 1 WHERE r.pollId = :pollId AND r.optionId = :optionId")
    void incrementVoteCount(@Param("pollId") Long pollId, @Param("optionId") Long optionId);

}
