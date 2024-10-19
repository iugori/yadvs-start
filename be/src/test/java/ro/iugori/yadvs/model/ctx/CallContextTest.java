package ro.iugori.yadvs.model.ctx;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CallContextTest {

    @Test
    void nextUuid() {
        assertTrue(CallContext.nextUuid().length() <= 22);
    }

}