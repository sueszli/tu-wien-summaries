package dslab;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Simulates reading lines from an {@link InputStream}.
 * <p>
 * Internally, the lines read from the underlying {@link InputStream} are buffered and can be retrieved on demand for
 * verification purposes.
 */
public class TestInputStream extends InputStream {
    private volatile BlockingQueue<String> lines = new LinkedBlockingQueue<>();
    private InputStream in;

    @Override
    public synchronized int read() throws IOException {
        if (in == null) {
            if ((in = nextLine()) == null) {
                return -1;
            }
        } else if (in.available() <= 0) {
            try {
                return -1;
            } finally {
                in = null;
            }
        }
        return in != null ? in.read() : -1;
    }

    /**
     * Returns a copy of the lines available for reading.
     *
     * @return the available lines
     * @throws IOException if the stream is closed
     */
    public List<String> getLines() throws IOException {
        List<String> copy = new ArrayList<>();
        do {
            try {
                copy.add(lines.take());
            } catch (InterruptedException e) {
                throw new IOException(e);
            }
        } while (lines.size() > 0);
        return copy;
    }

    /**
     * Adds the given line to the input queue.
     *
     * @param line the line to add
     */
    public void addLine(String line) {
        if (lines != null) {
            lines.add(line);
        }
    }

    /**
     * Prepares the next line available for reading from it.
     * <p/>
     * This method blocks until a line is available or the stream becomes closed.
     *
     * @return the {@link InputStream} holding the line
     * @throws IOException if the stream is closed
     */
    private InputStream nextLine() throws IOException {
        try {
            String line = null;
            while (lines != null && line == null) {
                line = lines.poll(500L, TimeUnit.MILLISECONDS);
            }
            if (line != null) {
                return new ByteArrayInputStream((line.endsWith("\n") ? line : line + '\n').getBytes());
            } else {
                return new ByteArrayInputStream("".getBytes());
            }
        } catch (InterruptedException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void close() throws IOException {
        if (in != System.in) {
            super.close();
        }
        lines = null;
        Thread.currentThread().interrupt();
    }
}
