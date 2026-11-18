package dslab.transfer;

/**
 * The transfer server is responsible for accepting mails sent by users, and forward them to mailbox servers via DMTP.
 * It also reports usage statistics to the monitoring server.
 * <p>
 * Do not change the existing method signatures!
 */
public interface ITransferServer extends Runnable {

    /**
     * Starts the server.
     */
    @Override
    void run();

    /**
     * CLI command to shut down the server. After this method, all resources should be closed, and the application
     * should terminate.
     */
    void shutdown();
}
