package ro.yugori.yadvs.api.vote;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import ro.yugori.yadvs.api.Setup;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class VoteTesting {

    public static final String VOTES_URI = Setup.ROOT_REST_URI + "/votes";

}
