package dslab.util.worker;

import dslab.util.worker.abstracts.Worker;

import java.net.Socket;


public interface ITCPWorkerFactory {

    /**
     * Creates a new Worker instance.7
     *
     * @param socket The socket the socket the worker will handle.
     * @return The created worker.
     */
    Worker newWorker(Socket socket);
}
