package dslab.client;

import dslab.*;
import dslab.util.Config;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ErrorCollector;

import java.util.concurrent.CountDownLatch;

import static org.hamcrest.CoreMatchers.is;

/**
 * Tests that the message client connects to the configured DMAP server at startup and sends the startsecure command.
 */
public class MessageClientStartupTest {

    @Rule
    public ErrorCollector err = new ErrorCollector();

    private SimpleTcpServer dmapServer;
    private Thread serverThread;
    private String clientId = "client-trillian";


    @Before
    public void setUp() throws Exception {
        Config clientConfig = new Config(clientId);
        int port = clientConfig.getInt("mailbox.port");
        dmapServer = new SimpleTcpServer(port);

        serverThread = new Thread(dmapServer);
        serverThread.start();

        Sockets.waitForSocket("localhost", port, Constants.COMPONENT_STARTUP_WAIT);
    }

    @After
    public void tearDown() throws Exception {
        dmapServer.close();
        serverThread.join(Constants.COMPONENT_TEARDOWN_WAIT);
    }

    @Test(timeout = 15000)
    public void startClient_shouldConnectToMailboxServerAndSendStartsecure() throws Exception {
        final CountDownLatch connected = new CountDownLatch(1);

        // setup mock server
        dmapServer.setSocketAcceptor(socket -> {
            try (JunitSocketClient client = new JunitSocketClient(socket)) {
                client.send("ok DMAP2.0");
                err.checkThat("expected first command from client to be startsecure", client.read(), is("startsecure"));

                connected.countDown();
                // the server unexpectedly terminates the connection here. make sure your client can handle it!
            } finally {
                dmapServer.close();
            }
        });

        // setup message client
        TestInputStream messageClientIn = new TestInputStream();
        TestOutputStream messageClientOut = new TestOutputStream();

        Runnable messageClient = ComponentFactory.createMessageClient(clientId, messageClientIn, messageClientOut);
        Thread messClientThread = new Thread(messageClient);
        messClientThread.start();

        // shutdown message client once the connection has been made
        connected.await();
        messageClientIn.addLine("shutdown");

        try {
            messClientThread.join(Constants.COMPONENT_TEARDOWN_WAIT);
        } catch (InterruptedException e) {
            // ignore
        }
    }
}
