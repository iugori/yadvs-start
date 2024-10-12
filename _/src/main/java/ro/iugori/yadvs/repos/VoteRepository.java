package ro.iugori.yadvs.repos;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.iugori.yadvs.domain.Vote;


public interface VoteRepository extends JpaRepository<Vote, Long> {
}
