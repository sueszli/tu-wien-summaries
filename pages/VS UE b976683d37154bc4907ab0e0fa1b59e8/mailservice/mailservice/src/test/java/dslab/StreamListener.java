package dslab;

import java.io.*;
import java.net.SocketException;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Runnable that reads from a Socket's InputStream line-by-line and writes it to a queue, until the socket is closed.
 */
public class StreamListener implements Runnable, Closeable {

    public static final String NULL_CHAR = new String(new byte[]{0x0});

    private InputStream in;
    private LinkedBlockingQueue<String> queue;

    public StreamListener(InputStream in) {
        this.in = in;
        this.queue = new LinkedBlockingQueue<>();
    }

    private static String removeNullBytes(String str) {
        return str.replace(NULL_CHAR, "");
    }

    public String poll(long timeout, TimeUnit timeUnit) {
        try {
            return queue.poll(timeout, timeUnit);
        } catch (InterruptedException e) {
            return null;
        }
    }

    public String listen(long timeout, TimeUnit timeUnit) {
        StringBuilder str = new StringBuilder(128);

        String line;
        while ((line = poll(timeout, timeUnit)) != null) {
            str.append(removeNullBytes(line)).append("\n");
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

    @Override
    public void run() {
        BufferedReader reader = new BufferedReader(new InputStreamReader(in));

        String line;
        try {
            while ((line = reader.readLine()) != null) {
                queue.offer(line);
            }
        } catch (SocketException e) {
            // socket closed
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close() throws IOException {
        in.close();
    }

}
