package ro.iugori.yadvs.model.domain;

public enum PollAction {

    DRAFTED,
    ACTIVATED,
    SUSPENDED,
    CLOSED,
    ARCHIVED,
    ;

    public static PollAction forStatus(PollStatus status) {
        return switch (status) {
            case DRAFT -> PollAction.DRAFTED;
            case ACTIVE -> PollAction.ACTIVATED;
            case SUSPENDED -> PollAction.SUSPENDED;
            case CLOSED -> PollAction.CLOSED;
            case ARCHIVED -> PollAction.ARCHIVED;
        };
    }

}
