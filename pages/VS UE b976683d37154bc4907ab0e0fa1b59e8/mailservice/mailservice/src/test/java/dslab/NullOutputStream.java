package dslab;

import java.io.OutputStream;

/**
 * This {@link OutputStream} has no destination (file/socket etc.) and all bytes written to it are ignored and lost.
 */
public final class NullOutputStream extends OutputStream {
    public static final OutputStream INSTANCE = new NullOutputStream();

    private NullOutputStream() {
    }

    public static OutputStream getInstance() {
        return INSTANCE;
    }

    /**
     * Discards the specified byte.
     */
    @Override
    public void write(int b) {
    }

    /**
     * Discards the specified byte array.
     */
    @Override
    public void write(byte[] b) {
    }

    /**
     * Discards the specified byte array.
     */
    @Override
    public void write(byte[] b, int off, int len) {
    }
}
