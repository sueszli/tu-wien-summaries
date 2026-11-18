package dslab.transfer;

import at.ac.tuwien.dsg.orvell.Shell;
import at.ac.tuwien.dsg.orvell.StopShellException;
import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.ComponentFactory;
import dslab.nameserver.INameserverRemote;
import dslab.util.Config;
import dslab.util.worker.ITCPWorkerFactory;
import dslab.util.workerManager.IWorkerManager;
import dslab.util.workerManager.WorkerManager;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;

public class TransferServer implements ITransferServer, Runnable {

    private final Config config;
    private final Shell shell;

    private Boolean shutdown = false;
    private ExecutorService forwardPool;
    private ForwardService forwardService;
    private ExecutorService connectionPool;
    private IWorkerManager workerManager;

    private INameserverRemote rootNs;

    /**
     * Creates a new server instance.
     *
     * @param componentId the id of the component that corresponds to the Config resource
     * @param config      the component config
     * @param in          the input stream to read console input from
     * @param out         the output stream to write console output to
     */
    public TransferServer(String componentId, Config config, InputStream in, PrintStream out) {
        this.config = config;

        shell = new Shell(in, out);
        shell.register(this);
        shell.setPrompt(componentId + "> ");
    }

    public static void main(String[] args) throws Exception {
        ITransferServer server = ComponentFactory.createTransferServer(args[0], System.in, System.out);
        server.run();
    }

    @Override
    public void run() {

        int port = config.getInt("tcp.port");
        try (ServerSocket serverSocket = new ServerSocket(port)) {
            System.out.println("Listening on port: " + serverSocket.getLocalPort());

            //find root ns for domain lookup
            getRootNs();

            MonitoringService monitoringService = getMonitoringService();

            forwardPool = Executors.newFixedThreadPool(10);
            connectionPool = Executors.newCachedThreadPool();

            InetAddress inetAddress = serverSocket.getInetAddress();
            forwardService = new ForwardService(forwardPool, monitoringService, inetAddress, rootNs);
            ITCPWorkerFactory factory = new TransferDMTPWorkerFactory(forwardService, inetAddress, rootNs);

            workerManager = new WorkerManager(factory, connectionPool);


            Thread loopThread = new Thread(() -> {
                while (!shutdown) {
                    Socket newConn;
                    try {
                        newConn = serverSocket.accept();
                        workerManager.addWorker(newConn);
                    } catch (IOException e) {
                        if (shutdown) {
                            continue;
                        }
                        throw new RuntimeException("Error accepting client connection", e);
                    }
                }
            });
            loopThread.start();

            shell.run();
            // interrupt thread to avoid deadlock
            loopThread.interrupt();
        } catch (IOException | NotBoundException e) {
            e.printStackTrace();
        } finally {
            if (!shutdown) {
                try {
                    shutdown();
                } catch (StopShellException ignored) {
                }
            }
        }
        System.out.println("Transfer-Server stopped.");
    }

    private MonitoringService getMonitoringService() {
        String monitoringHost = config.getString("monitoring.host");
        int monitoringPort = config.getInt("monitoring.port");
        MonitoringService monitoringService = new MonitoringService(monitoringHost, monitoringPort);
        return monitoringService;
    }

    private void getRootNs() throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(config.getString("registry.host"), config.getInt("registry.port"));
        rootNs = (INameserverRemote) registry.lookup(config.getString("root_id"));
    }

    @Override
    @Command
    public void shutdown() {
        shutdown = true;

        if (forwardPool != null) forwardPool.shutdownNow();
        if (forwardService != null) forwardService.shutdown();
        if (connectionPool != null) connectionPool.shutdownNow();
        if (workerManager != null) workerManager.shutdown();

        throw new StopShellException();
    }

    @Command
    public String connStatus() {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) connectionPool;

        return "------\n" + "Threadpool for request execution \n" + "ActiveThreads: " + executor.getActiveCount() + " \n" + "Queue: " + executor.getQueue().size() + " \n" + "PoolSize " + executor.getPoolSize() + " \n" + "-----";
    }

    @Command
    public String forwardStatus() {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) forwardPool;

        return "------\n" + "Threadpool for email forwarding\n" + "MaxThreads: " + executor.getMaximumPoolSize() + " \n" + "ActiveThreads: " + executor.getActiveCount() + " \n" + "Queue: " + executor.getQueue().size() + " \n" + "PoolSize " + executor.getPoolSize() + " \n" + "-----";
    }

    @Command
    public void connStatusTemp() {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) connectionPool;

        String res = "------\n" + "Threadpool for request execution \n" + "ActiveThreads: " + executor.getActiveCount() + " \n" + "Queue: " + executor.getQueue().size() + " \n" + "PoolSize " + executor.getPoolSize() + " \n" + "-----";
        System.out.println(res);
    }

    @Command
    public void forwardStatusTemp() {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) forwardPool;
        String res = "------\n" + "Threadpool for email forwarding\n" + "MaxThreads: " + executor.getMaximumPoolSize() + " \n" + "ActiveThreads: " + executor.getActiveCount() + " \n" + "Queue: " + executor.getQueue().size() + " \n" + "PoolSize " + executor.getPoolSize() + " \n" + "-----";

        System.out.println(res);
    }

    @Command
    public String forwardThreads() {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) forwardPool;
        return "ActiveThreads: " + executor.getActiveCount();
    }

    @Command
    public void setMaxForwardThreads(int count) {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) forwardPool;
        executor.setMaximumPoolSize(count);
    }


}
