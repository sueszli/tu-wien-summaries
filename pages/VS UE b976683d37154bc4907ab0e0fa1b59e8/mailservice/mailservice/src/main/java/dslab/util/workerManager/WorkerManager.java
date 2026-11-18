package dslab.util.workerManager;

import dslab.util.worker.AfterExecuteHookTask;
import dslab.util.worker.ITCPWorkerFactory;
import dslab.util.worker.abstracts.Worker;

import java.net.Socket;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;

public class WorkerManager implements IWorkerManager {

    private final ConcurrentLinkedQueue<Worker> workers = new ConcurrentLinkedQueue<>();
    private boolean shutdown = false;
    private ITCPWorkerFactory workerFactory;
    private ExecutorService workerPool;

    public WorkerManager(ITCPWorkerFactory workerFactory, ExecutorService workerPool) {
        this.workerFactory = workerFactory;
        this.workerPool = workerPool;
    }

    public WorkerManager(ExecutorService workerPool) {
        this.workerPool = workerPool;
    }

    public WorkerManager() {
    }

    @Override
    public void addWorker(Socket socket) {
        addWorker(socket, this.workerFactory);
    }

    @Override
    public void addWorker(Socket socket, ITCPWorkerFactory workerFactory) {
        addWorker(socket, workerFactory, workerPool);
    }

    @Override
    public void addWorker(Socket socket, ITCPWorkerFactory factory, ExecutorService pool) {
//        if (shutdown) throw new RuntimeException("Manager has been shutdown");
        if (shutdown) return;
        Worker worker = factory.newWorker(socket);
        workers.add(worker);
        AfterExecuteHookTask wrappedWorker = new AfterExecuteHookTask(worker, () -> workers.remove(worker));
        pool.execute(wrappedWorker);
    }


    public void shutdown() {
        shutdown = true;
        for (Worker worker : workers) {
            worker.quit();
        }
    }
}
