package dslab.util.generator;

import java.util.concurrent.atomic.AtomicInteger;

public class Generator {

    private final static AtomicInteger counter = new AtomicInteger();

    public static long getID() {
        return counter.getAndIncrement();
    }

}
