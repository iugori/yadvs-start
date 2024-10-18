package ro.iugori.yadvs.model.ctx;

import com.devskiller.friendly_id.FriendlyId;
import lombok.Getter;
import org.slf4j.Logger;

import java.time.LocalDateTime;
import java.util.UUID;

public class CallContext {

    public static String nextUuid() {
        return FriendlyId.toFriendlyId(UUID.randomUUID());
    }

    @Getter
    private final String traceId = nextUuid();

    @Getter
    private final LocalDateTime traceTs = LocalDateTime.now();

    @Getter
    private final Logger logger;

    public CallContext(Logger logger) {
        this.logger = logger;
    }

}