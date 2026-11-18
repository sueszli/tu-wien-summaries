package dslab;

import dslab.client.IMessageClient;
import dslab.client.MessageClient;
import dslab.mailbox.IMailboxServer;
import dslab.mailbox.MailboxServer;
import dslab.monitoring.IMonitoringServer;
import dslab.monitoring.MonitoringRepository;
import dslab.monitoring.MonitoringServer;
import dslab.nameserver.INameserver;
import dslab.nameserver.Nameserver;
import dslab.transfer.ITransferServer;
import dslab.transfer.TransferServer;
import dslab.util.Config;

import java.io.InputStream;
import java.io.PrintStream;

/**
 * The component factory provides methods to create the core components of the application. You can edit the method body
 * if the component instantiation requires additional logic.
 * <p>
 * Do not change the existing method signatures!
 */
public final class ComponentFactory {

    private ComponentFactory() {
        // static utility class
    }

    /**
     * Creates a new {@link IMonitoringServer} instance.
     *
     * @param componentId the component id
     * @param in          the input stream used for accepting cli commands
     * @param out         the output stream to print to
     * @return a new MonitoringServer instance
     */
    public static IMonitoringServer createMonitoringServer(String componentId, InputStream in, PrintStream out) throws Exception {
        Config config = new Config(componentId);
        MonitoringRepository repo = new MonitoringRepository();
        return new MonitoringServer(componentId, config, in, out, repo);
    }

    /**
     * Creates a new {@link IMailboxServer} instance.
     *
     * @param componentId the component id
     * @param in          the input stream used for accepting cli commands
     * @param out         the output stream to print to
     * @return a new MailboxServer instance
     */
    public static IMailboxServer createMailboxServer(String componentId, InputStream in, PrintStream out) throws Exception {
        Config config = new Config(componentId);
        return new MailboxServer(componentId, config, in, out);
    }

    /**
     * Creates a new {@link ITransferServer} instance.
     *
     * @param componentId the component id
     * @param in          the input stream used for accepting cli commands
     * @param out         the output stream to print to
     * @return a new TransferServer instance
     */
    public static ITransferServer createTransferServer(String componentId, InputStream in, PrintStream out) throws Exception {
        Config config = new Config(componentId);
        return new TransferServer(componentId, config, in, out);
    }


    /**
     * Creates a new {@link INameserver} instance.
     *
     * @param componentId the component id
     * @param in          the input stream used for accepting cli commands
     * @param out         the output stream to print to
     * @return a new Nameserver instance
     */
    public static INameserver createNameserver(String componentId, InputStream in, PrintStream out) throws Exception {
        Config config = new Config(componentId);
        return new Nameserver(componentId, config, in, out);
    }

    /**
     * Creates a new {@link IMessageClient} instance.
     *
     * @param componentId the component id
     * @param in          the input stream used for accepting cli commands
     * @param out         the output stream to print to
     * @return a new MessageClient instance
     */
    public static IMessageClient createMessageClient(String componentId, InputStream in, PrintStream out) throws Exception {
        Config config = new Config(componentId);
        return new MessageClient(componentId, config, in, out);
    }
}
