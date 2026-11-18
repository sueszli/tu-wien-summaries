package dslab.monitoring;

import at.ac.tuwien.dsg.orvell.Shell;
import at.ac.tuwien.dsg.orvell.StopShellException;
import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.ComponentFactory;
import dslab.util.Config;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.DatagramSocket;

public class MonitoringServer implements IMonitoringServer {

    private final Config config;
    private final Shell shell;
    private final MonitoringRepository repo;
    private DatagramSocket datagramSocket;
    private PackageListener listener;

    /**
     * Creates a new server instance.
     *
     * @param componentId the id of the component that corresponds to the Config resource
     * @param config      the component config
     * @param in          the input stream to read console input from
     * @param out         the output stream to write console output to
     */
    public MonitoringServer(String componentId, Config config, InputStream in, PrintStream out, MonitoringRepository repo) {
        this.config = config;
        this.repo = repo;

        shell = new Shell(in, out);
        shell.register(this);
        shell.setPrompt(componentId + "> ");
    }

    public static void main(String[] args) throws Exception {
        IMonitoringServer server = ComponentFactory.createMonitoringServer(args[0], System.in, System.out);
        server.run();
    }

    @Override
    public void run() {
        openServer();
        System.out.println("Listening on port: " + this.datagramSocket.getLocalPort());

        listener = new PackageListener(datagramSocket, repo);
        listener.start();

        shell.run();

        if (!datagramSocket.isClosed()) shutdown();
    }

    @Override
    @Command
    public String addresses() {
        return repo.getFormattedAddresses();
    }

    @Override
    @Command
    public String servers() {
        return repo.getFormattedServers();
    }

    @Override
    @Command
    public String shutdown() {
        if (listener != null) listener.shutdown();
        datagramSocket.close();
        throw new StopShellException();
    }

    private void openServer() {
        try {
            // constructs a datagram socket and binds it to the specified port
            datagramSocket = new DatagramSocket(config.getInt("udp.port"));
        } catch (IOException e) {
            throw new RuntimeException("Cannot listen on UDP port.", e);
        }
    }

}
