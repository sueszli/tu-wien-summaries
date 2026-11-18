package dslab;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Simulates writing lines to an {@link PrintStream}.
 * <p/>
 * Internally, the lines written to the underlying {@link PrintStream} are buffered and can be retrieved on demand for
 * verification purposes.
 */
public class TestOutputStream extends PrintStream {
    private final LinkedBlockingQueue<String> lines = new LinkedBlockingQueue<>();
    private volatile StringBuilder line = new StringBuilder();
    private PrintStream delegate;

    /**
     * Creates a new {@code TestOutputStream} instance writing to an {@link NullOutputStream}.
     */
    public TestOutputStream() {
        this(new PrintStream(NullOutputStream.getInstance()));
    }

    /**
     * Creates a new {@code TestOutputStream} instance writing to the provided {@link PrintStream}.
     *
     * @param delegate the stream to write to
     */
    public TestOutputStream(PrintStream delegate) {
        super(delegate);
        this.delegate = delegate;
    }

    @Override
    public void close() {
        if (delegate != System.out) {
            super.close();
        }
    }

    @Override
    public void write(int b) {
        delegate.write(b);
        if (b == '\r') {
            // Do nothing
        } else if (b == '\n') {
            addLine();
        } else {
            line.append((char) b);
        }
    }

    public void write(byte b[], int off, int len) {
        if (b == null) {
            throw new NullPointerException();
        } else if ((off < 0) || (off > b.length) || (len < 0) || ((off + len) > b.length) || ((off + len) < 0)) {
            throw new IndexOutOfBoundsException();
        } else if (len == 0) {
            return;
        }
        for (int i = 0; i < len; i++) {
            write(b[off + i]);
        }
    }

    /**
     * Returns a copy of the lines written to the {@link PrintStream} so far.
     *
     * @return the written lines
     */
    public List<String> getLines() {
        synchronized (lines) {
            if (line.length() > 0) {
                addLine();
            }
            return new ArrayList<>(lines);
        }
    }

    /**
     * Listens for stream output until no output has been received for one second.
     *
     * @return the aggregated output (joined by a newline)
     * @throws InterruptedException if the polling was interrupted
     */
    public String listen() throws InterruptedException {
        return listen(1, TimeUnit.SECONDS);
    }

    public String listen(long timeout, TimeUnit timeUnit) throws InterruptedException {
        StringBuilder str = new StringBuilder(128);

        String line;
        while ((line = poll(timeout, timeUnit)) != null) {
            str.append(line).append("\n");
        }

        if (str.length() > 0) {
            // remove trailing whitespace
            int i = str.length() - 1;
            if ('\n' == str.charAt(i)) {
                str.deleteCharAt(i);
            }
        }

        return str.toString();
    }

    public String poll(long time, TimeUnit timeUnit) throws InterruptedException {
        return lines.poll(time, timeUnit);
    }

    /**
     * Returns a copy of the lines written to the {@link PrintStream} so far and clears the buffer.
     *
     * @return the written lines
     * @see #getLines()
     * @see #clear()
     */
    public List<String> reset() {
        synchronized (lines) {
            List<String> lines = getLines();
            clear();
            return lines;
        }
    }

    /**
     * Clears the buffer holding the lines written to the {@link PrintStream} so far.
     */
    private void clear() {
        synchronized (lines) {
            lines.clear();
            line = new StringBuilder();
        }
    }

    /**
     * Appends the current line to the buffer.
     */
    private void addLine() {
        synchronized (lines) {
            lines.add(line.toString());
            line = new StringBuilder();
        }
    }
}
