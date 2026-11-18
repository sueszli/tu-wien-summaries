package dslab.util.workerManager;

import dslab.util.worker.ITCPWorkerFactory;

import java.net.Socket;
import java.util.concurrent.ExecutorService;

public interface IWorkerManager {

    /**
     * Adds a worker to be managed.
     *
     * @param socket The socket the worker should handle.
     * @return
     */
    void addWorker(Socket socket);

    /**
     * Adds a worker, created by the workerFactory, to be managed.
     *
     * @param socket        The socket the worker should handle.
     * @param workerFactory The factory used to create the worker.
     */
    void addWorker(Socket socket, ITCPWorkerFactory workerFactory);

    void addWorker(Socket socket, ITCPWorkerFactory workerFactory, ExecutorService pool);

    /**
     * Shuts down the worker manager and all its managed workers.
     */
    void shutdown();
}
