package dslab;

import org.hamcrest.Matcher;
import org.junit.Assert;
import org.junit.rules.ErrorCollector;

import java.io.Closeable;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.CoreMatchers.containsString;

public class JunitSocketClient implements Closeable {

    private ErrorCollector err;

    private Socket socket;
    private PrintWriter writer;

    private StreamListener listener;

    /**
     * Creates a new Socket that connects to localhost on the given port and holds the I/O resources.
     *
     * @param port the port to connect to
     * @throws IOException if an I/O error occurred while connecting.
     */
    public JunitSocketClient(int port) throws IOException {
        this(new Socket("127.0.0.1", port));
    }

    public JunitSocketClient(Socket socket) throws IOException {
        this.socket = socket;
        this.writer = new PrintWriter(socket.getOutputStream());
        this.listener = new StreamListener(socket.getInputStream());

        new Thread(listener).start();
    }

    /**
     * Creates a new Socket that connects to localhost on the given port and holds the I/O resources.
     *
     * @param port the port to connect to
     * @param err  the error collector used to verify communication
     * @throws IOException if an I/O error occurred while connecting.
     */

    public JunitSocketClient(int port, ErrorCollector err) throws IOException {
        this(port);
        this.err = err;
    }

    public Socket getSocket() {
        return socket;
    }

    public PrintWriter getWriter() {
        return writer;
    }

    public void send(String message) {
        writer.println(message);
        writer.flush();
    }

    public String sendAndListen(String message) {
        send(message);
        return listen();
    }

    public String listen() {
        return listen(1, TimeUnit.SECONDS);
    }

    public String listen(long time, TimeUnit timeUnit) {
        return listener.listen(time, timeUnit);
    }

    public String read() throws IOException {
        return listener.poll(1, TimeUnit.MINUTES);
    }

    /**
     * Reads a line from the input stream and verifies that it contains the given string.
     *
     * @param string the partial string to match
     * @throws IOException on read errors
     */
    public void verify(String string) throws IOException {
        assertThat(read(), containsString(string));
    }

    /**
     * Writes the given string to the output stream, and then behaves like {@link #verify(String)}.
     *
     * @param request  the request to send
     * @param response the expected response (partial string match)
     * @throws IOException on I/O errors
     */
    public void sendAndVerify(String request, String response) throws IOException {
        assertThat(sendAndRead(request), containsString(response));
    }

    public String sendAndRead(String message) throws IOException {
        send(message);
        return read();
    }

    @Override
    public void close() throws IOException {
        closeQuietly(listener);
        closeQuietly(writer);
        closeQuietly(socket);
    }

    private <T> void assertThat(T actual, Matcher<? super T> matcher) {
        if (err != null) {
            err.checkThat(actual, matcher);
        } else {
            Assert.assertThat(actual, matcher);
        }
    }

    private void closeQuietly(Closeable closeable) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (IOException e) {
                // ignore
            }
        }
    }
}
