package dslab;

import java.io.IOException;
import java.net.*;

/**
 * Util class for checking sockets.
 */
public final class Sockets {

    private Sockets() {
        // util class
    }

    /**
     * Waits a given time for a TCP server socket to be opened at the given host and port by repeatedly (every 10 ms)
     * trying to connect to the address.
     *
     * @param host the expected server socket host
     * @param port the expected server socket port
     * @param ms   the time in milliseconds.
     * @throws SocketTimeoutException if the timeout period was reached
     */
    public static void waitForSocket(String host, int port, long ms) throws SocketTimeoutException {
        long interval = 10;

        while (ms > 0) {
            try (Socket ignored = new Socket(host, port)) {
                return;
            } catch (IOException e) {
                ms -= interval;

                try {
                    Thread.sleep(interval);
                } catch (InterruptedException e1) {
                    break;
                }
            }
        }

        throw new SocketTimeoutException("Gave up waiting for socket " + host + ":" + port);
    }

    /**
     * Tries to open a DatagramSocket on the given port. If a BindException is thrown, it indicates that the socket was
     * open before, and the method will return true. If the socket was opened successfully, the method returns false and
     * immediately closes the socket.
     *
     * @param port the local port
     * @return true if the socket was already open
     * @throws SocketException if the socket could not be opened due to some other reason than a bind exception
     */
    public static boolean isDatagramSocketOpen(int port) throws SocketException {
        try (DatagramSocket socket = new DatagramSocket(port)) {
            return false;
        } catch (BindException e) {
            return true;
        }
    }

    /**
     * Tries to open a ServerSocket on the given port. If a BindException is thrown, it indicates that the socket was
     * open before, and the method will return true. If the socket was opened successfully, the method returns false and
     * immediately closes the socket.
     *
     * @param port the local port
     * @return true if the socket was already open
     * @throws IOException if the socket could not be opened due to some other reason than a bind exception
     */
    public static boolean isServerSocketOpen(int port) throws IOException {
        try (ServerSocket socket = new ServerSocket(port)) {
            return false;
        } catch (BindException e) {
            return true;
        }
    }


}
