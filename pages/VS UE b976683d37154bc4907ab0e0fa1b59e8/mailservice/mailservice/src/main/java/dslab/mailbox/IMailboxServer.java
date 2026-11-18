package dslab.mailbox;

/**
 * The mailbox server receives mails via DMTP from transfer servers, and makes them available to users via the DMAP
 * protocol.
 * <p>
 * Do not change the existing method signatures!
 */
public interface IMailboxServer extends Runnable {

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
