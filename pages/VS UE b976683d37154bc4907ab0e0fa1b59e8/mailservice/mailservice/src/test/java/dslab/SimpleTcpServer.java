package dslab;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.Closeable;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

/**
 * Used for mocking purposes, this class provides a simple control loop over a ServerSocket.
 */
public class SimpleTcpServer implements Runnable, Closeable {

    private static final Log LOG = LogFactory.getLog(SimpleTcpServer.class);

    private final int port;
    private CheckedConsumer<Socket, IOException> socketAcceptor;

    private ServerSocket serverSocket;

    public SimpleTcpServer(int port) {
        this.port = port;
    }

    public synchronized CheckedConsumer<Socket, IOException> getSocketAcceptor() {
        return socketAcceptor;
    }

    public void setSocketAcceptor(CheckedConsumer<Socket, IOException> socketAcceptor) {
        this.socketAcceptor = socketAcceptor;
    }

    @Override
    public void run() {
        try {
            LOG.info("Starting mock server on " + port);
            serverSocket = new ServerSocket(port);

            while (true) {
                Socket connection;
                try {
                    LOG.info("Listening on client connection");
                    connection = serverSocket.accept();
                } catch (SocketException e) {
                    break;
                }

                try {
                    LOG.info("Processing client connection " + connection);
                    onAccept(connection);
                } catch (SocketException e) {
                    continue;
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            }

        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            close();
        }
    }

    @Override
    public void close() {
        if (serverSocket != null) {
            try {
                LOG.info("Closing server socket");
                serverSocket.close();
            } catch (IOException e) {
                LOG.warn("Error closing server socket", e);
            }
        }
    }

    protected void onAccept(Socket socket) throws IOException {
        if (socketAcceptor != null) {
            socketAcceptor.accept(socket);
        }
    }
}
