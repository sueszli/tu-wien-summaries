package dslab.client;

import java.io.IOException;

/**
 * A Message Client application.
 * <p>
 * Do not change the existing method signatures!
 */
public interface IMessageClient extends Runnable {

    /**
     * Starts the message client.
     */
    @Override
    void run();

    /**
     * Outputs the contents of the user's inbox on the shell.
     */
    void inbox();

    /**
     * Deletes the mail with the given id. Prints 'ok' if the mail was deleted successfully, 'error {explanation}'
     * otherwise.
     *
     * @param id the mail id
     */
    void delete(String id) throws IOException;

    /**
     * Verifies the signature of the message by calculating its hash value using the shared secret. Prints 'ok' if the
     * message integrity was successfully verified, or 'error' otherwise.
     *
     * @param id the message id
     */
    void verify(String id) throws IOException;

    /**
     * Sends a message from the mail client's user to the given recipient(s)
     *
     * @param to      comma separated list of recipients
     * @param subject the message subject
     * @param data    the message data
     */
    void msg(String to, String subject, String data) throws IOException;

    /**
     * Shuts down the application.
     */
    void shutdown();
}
