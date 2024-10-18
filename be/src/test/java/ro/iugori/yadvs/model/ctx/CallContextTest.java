package ro.iugori.yadvs.model.ctx;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CallContextTest {

    @Test
    void nextUuid() {
        assertEquals(22, CallContext.nextUuid().length());
    }

}