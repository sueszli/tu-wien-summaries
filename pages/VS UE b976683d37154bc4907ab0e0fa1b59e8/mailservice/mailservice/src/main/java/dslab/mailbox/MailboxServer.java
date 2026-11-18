package dslab.mailbox;

import at.ac.tuwien.dsg.orvell.Shell;
import at.ac.tuwien.dsg.orvell.StopShellException;
import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.ComponentFactory;
import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.mailbox.repository.MailboxDataRepository;
import dslab.nameserver.AlreadyRegisteredException;
import dslab.nameserver.INameserverRemote;
import dslab.nameserver.InvalidDomainException;
import dslab.util.Config;
import dslab.util.security.KeyType;
import dslab.util.security.RSAKeyloader;
import dslab.util.worker.ITCPWorkerFactory;
import dslab.util.worker.ProtocolType;
import dslab.util.workerManager.IWorkerManager;
import dslab.util.workerManager.WorkerManager;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.security.PrivateKey;
import java.security.spec.InvalidKeySpecException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;

public class MailboxServer implements IMailboxServer, Runnable {

    private final ExecutorService dmapConnectionPool = Executors.newCachedThreadPool();
    private final ExecutorService dmtpConnectionPool = Executors.newFixedThreadPool(10);
    private final IMailboxDataRepository dataRepository;
    private final String componentId;
    private final Shell shell;
    private final Config config;
    private ServerSocket dmtpServerSocket;
    private ServerSocket dmapServerSocket;
    private Boolean shutdown = false;
    private ITCPWorkerFactory dmapFactory;
    private ITCPWorkerFactory dmtpFactory;
    private IWorkerManager workerManager;

    /**
     * Creates a new server instance.
     *
     * @param componentId the id of the component that corresponds to the Config resource
     * @param config      the component config
     * @param in          the input stream to read console input from
     * @param out         the output stream to write console output to
     */
    public MailboxServer(String componentId, Config config, InputStream in, PrintStream out) {
        this.componentId = componentId;
        this.config = config;
        this.dataRepository = new MailboxDataRepository(new Config(config.getString("users.config")));

        shell = new Shell(in, out);
        shell.register(this);
        shell.setPrompt(componentId + "> ");
    }

    public static void main(String[] args) throws Exception {
        IMailboxServer server = ComponentFactory.createMailboxServer(args[0], System.in, System.out);
        server.run();
    }

    @Override
    public void run() {
        openServer();
        Config users = new Config(config.getString("users.config"));

        try {
            RSAKeyloader keyloader = new RSAKeyloader();
            PrivateKey privateKey = (PrivateKey) keyloader.loadKey(componentId, KeyType.PRIVATE);
            dmapFactory = new MailboxDMAPWorkerFactory(users, dataRepository, privateKey, componentId);
        } catch (IOException | InvalidKeySpecException e) {
            e.printStackTrace();
            if (!shutdown) {
                try {
                    shutdown();
                } catch (StopShellException ignored) {
                }
            }
        }
        dmtpFactory = new MailboxDMTPWorkerFactory(config, dataRepository);

        workerManager = new WorkerManager();

        System.out.println("Listening DMAP on port: " + this.dmapServerSocket.getLocalPort());
        System.out.println("Listening DMTP on port: " + this.dmtpServerSocket.getLocalPort());

        Thread dmapLoop = getRequestLoopThread(ProtocolType.DMAP);
        Thread dmtpLoop = getRequestLoopThread(ProtocolType.DMTP);

        dmapLoop.start();
        dmtpLoop.start();

        shell.run();
        if (!shutdown) {
            try {
                shutdown();
            } catch (StopShellException ignored) {
            }
        }
        // interrupt thread to avoid deadlock
        dmapLoop.interrupt();
        dmtpLoop.interrupt();

        System.out.println("Mailbox-Server stopped.");
    }

    @Override
    @Command
    public void shutdown() {
        System.out.println("Shutdown Mailbox-Server...");
        shutdown = true;
        closeServer();
        dmapConnectionPool.shutdownNow();
        dmtpConnectionPool.shutdownNow();
        workerManager.shutdown();
        throw new StopShellException();
    }

    public void openServer() {
        int dmapPort = config.getInt("dmap.tcp.port");
        int dmtpPort = config.getInt("dmtp.tcp.port");
        try {
            dmapServerSocket = new ServerSocket(dmapPort);
            dmtpServerSocket = new ServerSocket(dmtpPort);
        } catch (IOException e) {
            closeServer();
            throw new RuntimeException("Could not open port " + dmapPort + " or " + dmtpPort, e);
        }

        try {
            registerMailboxOnNameserver();
        } catch (NotBoundException | RemoteException e) {
            closeServer();
            throw new RuntimeException("Could not connect to nameserver: " + e.getMessage(), e);
        } catch (AlreadyRegisteredException e) {
            shell.out().println("Mailbox server with this domain was already registered. Conutinue...");
        } catch (InvalidDomainException e) {
            closeServer();
            throw new RuntimeException("Domain of mailboxserver is not valid: " + e.getMessage(), e);
        }
    }

    private void registerMailboxOnNameserver() throws NotBoundException, RemoteException, AlreadyRegisteredException, InvalidDomainException {
        Registry registry = LocateRegistry.getRegistry(config.getString("registry.host"), config.getInt("registry.port"));

        INameserverRemote nameserver = (INameserverRemote) registry.lookup(config.getString("root_id"));
        String address = dmtpServerSocket.getInetAddress().getHostAddress() + ":" + dmtpServerSocket.getLocalPort();
        nameserver.registerMailboxServer(config.getString("domain"), address);
    }

    public void closeServer() {
        try {
            if (this.dmapServerSocket != null) this.dmapServerSocket.close();

            if (this.dmtpServerSocket != null) this.dmtpServerSocket.close();

        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("Failed to close server", e);
        }
    }

    @Command
    public void dmtpStatus() {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) dmtpConnectionPool;
        System.out.println("------\n" + "Threadpool for DMTP requests \n" + "MaxPoolsize:" + executor.getMaximumPoolSize() + "\n" + "Threads: " + executor.getActiveCount() + " \n" + "Queue: " + executor.getQueue().size() + " \n" + "PoolSize " + executor.getPoolSize() + " \n" + "-----");
    }

    @Command
    public void dmapStatus() {
        ThreadPoolExecutor executor = (ThreadPoolExecutor) dmapConnectionPool;
        System.out.println("------\n" + "Threadpool for DMAP requests \n" + "Threads: " + executor.getActiveCount() + " \n" + "Queue: " + executor.getQueue().size() + " \n" + "PoolSize " + executor.getPoolSize() + " \n" + "-----");
    }

    @Command
    public void listUsers() {
        System.out.println(String.join("\n", new Config(config.getString("users.config")).listKeys()));
    }

    private Thread getRequestLoopThread(ProtocolType type) {
        return new Thread(() -> {
            while (!shutdown) {
                Socket newConn;
                try {
                    if (type == ProtocolType.DMAP) {
                        newConn = this.dmapServerSocket.accept();
                        workerManager.addWorker(newConn, dmapFactory, dmapConnectionPool);
                    } else {
                        newConn = this.dmtpServerSocket.accept();
                        workerManager.addWorker(newConn, dmtpFactory, dmtpConnectionPool);
                    }
                } catch (IOException e) {
                    if (shutdown) {
                        continue;
                    }

                    throw new RuntimeException("Error accepting client connection", e);
                }
            }
        });
    }
}
