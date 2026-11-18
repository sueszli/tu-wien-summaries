package dslab.client;

import dslab.*;
import dslab.nsHelper.NsSetupHelper;
import dslab.nsHelper.NsSetupHelperFactory;
import dslab.util.Config;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ErrorCollector;

import java.util.concurrent.TimeUnit;

import static org.hamcrest.CoreMatchers.*;

/**
 * Starts a mailbox server and a message client, and injects mails to the mailbox server that are read by the mail
 * client. Note that your mailbox server should run even if there is no naming service is available.
 */
public class MessageClientMailboxTest {

    private static final Log LOG = LogFactory.getLog(MessageClientMailboxTest.class);

    @Rule
    public ErrorCollector err = new ErrorCollector();

    private Config mailboxConfig;
    private TestInputStream mailboxServerIn;
    private TestOutputStream mailboxServerOut;
    private Thread mailboxServerThread;

    private TestInputStream messageClientIn;
    private TestOutputStream messageClientOut;
    private Thread messageClientThread;

    private NsSetupHelper nsHelper;

    @Before
    public void setUpNs() throws Exception {
        nsHelper = NsSetupHelperFactory.createDefaultNsHelper();
        nsHelper.startup();
    }

    @After
    public void shutdownNs() throws Exception {
        nsHelper.shutdown();
    }

    @Before
    public void setUp() throws Exception {
        LOG.info("Creating mailbox server");
        mailboxConfig = new Config("mailbox-earth-planet");
        mailboxServerIn = new TestInputStream();
        mailboxServerOut = new TestOutputStream();
        Runnable mailboxServer = ComponentFactory.createMailboxServer("mailbox-earth-planet", mailboxServerIn, mailboxServerOut);

        mailboxServerThread = new Thread(mailboxServer);
        mailboxServerThread.start();

        LOG.info("Waiting for mailbox server sockets");
        Sockets.waitForSocket("localhost", mailboxConfig.getInt("dmtp.tcp.port"), Constants.COMPONENT_STARTUP_WAIT);
        Sockets.waitForSocket("localhost", mailboxConfig.getInt("dmap.tcp.port"), Constants.COMPONENT_STARTUP_WAIT);

        LOG.info("Starting message client");
        messageClientIn = new TestInputStream();
        messageClientOut = new TestOutputStream();
        Runnable messageClient = ComponentFactory.createMessageClient("client-trillian", messageClientIn, messageClientOut);

        messageClientThread = new Thread(messageClient);
        messageClientThread.start();
        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);
    }

    @After
    public void tearDown() throws Exception {
        messageClientIn.addLine("shutdown");
        messageClientThread.join(Constants.COMPONENT_TEARDOWN_WAIT);

        mailboxServerIn.addLine("shutdown");
        mailboxServerThread.join(Constants.COMPONENT_TEARDOWN_WAIT);
    }

    @Test(timeout = 20000)
    public void inbox_singleMail_showsAllInboxDataCorrectly() throws Exception {

        try (JunitSocketClient client = new JunitSocketClient(mailboxConfig.getInt("dmtp.tcp.port"), err)) {
            err.checkThat("Expected mailbox server to implement DMTP2.0", client.read(), is("ok DMTP2.0"));
            client.sendAndVerify("begin", "ok");
            client.sendAndVerify("from arthur@earth.planet", "ok");
            client.sendAndVerify("to trillian@earth.planet", "ok");
            client.sendAndVerify("subject somesubject", "ok");
            client.sendAndVerify("data somedata", "ok");
            client.sendAndVerify("hash 98yUrgHu4BctmhAel19nUAhGRVdVh9qD7Ge3VJBiehk=", "ok"); // valid hash
            client.sendAndVerify("send", "ok");
            client.send("quit");
        }

        Thread.sleep(2000); // wait a bit

        messageClientIn.addLine("inbox");
        String output = messageClientOut.listen();

        err.checkThat(output, allOf(
                containsString("trillian@earth.planet"),
                containsString("arthur@earth.planet"),
                containsString("somesubject"),
                containsString("somedata")
        ));
    }

    @Test(timeout = 40000)
    public void inbox_mulipleMails_showsAllInboxDataCorrectly() throws Exception {

        // send a mail to trillian
        try (JunitSocketClient client = new JunitSocketClient(mailboxConfig.getInt("dmtp.tcp.port"), err)) {
            err.checkThat("Expected mailbox server to implement DMTP2.0", client.read(), is("ok DMTP2.0"));
            client.sendAndVerify("begin", "ok");
            client.sendAndVerify("from arthur@earth.planet", "ok");
            client.sendAndVerify("to trillian@earth.planet", "ok");
            client.sendAndVerify("subject somesubject", "ok");
            client.sendAndVerify("data somedata", "ok");
            client.sendAndVerify("hash 98yUrgHu4BctmhAel19nUAhGRVdVh9qD7Ge3VJBiehk=", "ok"); // valid hash
            client.sendAndVerify("send", "ok");
            client.send("quit");
        }

        // send another mail to trillian
        try (JunitSocketClient client = new JunitSocketClient(mailboxConfig.getInt("dmtp.tcp.port"), err)) {
            err.checkThat(client.read(), is("ok DMTP2.0"));
            client.sendAndVerify("begin", "ok");
            client.sendAndVerify("from zaphod@univer.ze", "ok");
            client.sendAndVerify("to trillian@earth.planet", "ok");
            client.sendAndVerify("subject zaphodsubject", "ok");
            client.sendAndVerify("data zaphoddata", "ok");
            client.sendAndVerify("hash 4Bctm9nHuVU9qe3VJBhiAhGR987GyUekrgdVhhAel1D=", "ok"); // invalid hash
            client.sendAndVerify("send", "ok");
            client.send("quit");
        }

        // send a mail to arthur
        try (JunitSocketClient client = new JunitSocketClient(mailboxConfig.getInt("dmtp.tcp.port"), err)) {
            err.checkThat(client.read(), is("ok DMTP2.0"));
            client.sendAndVerify("begin", "ok");
            client.sendAndVerify("from zaphod@univer.ze", "ok");
            client.sendAndVerify("to arthur@earth.planet", "ok");
            client.sendAndVerify("subject nottrilliansubject", "ok");
            client.sendAndVerify("data nottrilliandata", "ok");
            client.sendAndVerify("hash gdBctmhAeU9qel1DuV3VJBiUrhkA9Vhe4n987GyHhGR=", "ok"); // invalid hash
            client.sendAndVerify("send", "ok");
            client.send("quit");
        }

        Thread.sleep(2000); // wait a bit

        messageClientIn.addLine("inbox");
        String output = messageClientOut.listen(3, TimeUnit.SECONDS);

        err.checkThat("inbox output did not contain all data from first mail", output, allOf(
                containsString("trillian@earth.planet"),
                containsString("arthur@earth.planet"),
                containsString("somesubject"),
                containsString("somedata")
        ));
        err.checkThat("inbox output did not contain all data from second mail", output, allOf(
                containsString("zaphod@univer.ze"),
                containsString("zaphodsubject"),
                containsString("zaphoddata")
        ));
        err.checkThat("inbox output contained data from a different user", output, not(anyOf(
                containsString("nottrilliansubject"),
                containsString("nottrilliandata")
        )));
    }

}
