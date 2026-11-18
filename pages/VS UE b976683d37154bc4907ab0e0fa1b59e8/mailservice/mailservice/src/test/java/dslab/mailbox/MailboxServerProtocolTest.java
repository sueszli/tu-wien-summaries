package dslab.mailbox;

import dslab.*;
import dslab.nsHelper.NsSetupHelper;
import dslab.nsHelper.NsSetupHelperFactory;
import dslab.util.Config;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;

public class MailboxServerProtocolTest extends TestBase {

    private static final Log LOG = LogFactory.getLog(MailboxServerProtocolTest.class);

    private String componentId = "mailbox-earth-planet";

    private IMailboxServer component;
    private int dmapServerPort;
    private int dmtpServerPort;

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

        component = ComponentFactory.createMailboxServer(componentId, in, out);
        dmapServerPort = new Config(componentId).getInt("dmap.tcp.port");
        dmtpServerPort = new Config(componentId).getInt("dmtp.tcp.port");

        new Thread(component).start();

        LOG.info("Waiting for server sockets to appear");
        Sockets.waitForSocket("localhost", dmapServerPort, Constants.COMPONENT_STARTUP_WAIT);
        Sockets.waitForSocket("localhost", dmtpServerPort, Constants.COMPONENT_STARTUP_WAIT);
    }

    @After
    public void tearDown() throws Exception {

        in.addLine("shutdown"); // send "shutdown" command to command line
        nsHelper.shutdown();
        Thread.sleep(Constants.COMPONENT_TEARDOWN_WAIT);
    }

    @Test(timeout = 15000)
    public void loginAndLogout_withValidLogin() throws Exception {

        try (JunitSocketClient client = new JunitSocketClient(dmapServerPort, err)) {
            client.verify("ok DMAP");
            client.sendAndVerify("login trillian 12345", "ok");
            client.sendAndVerify("logout", "ok");
            client.sendAndVerify("quit", "ok bye");
        }
    }

    @Test(timeout = 15000)
    public void login_withInvalidLogin_returnsError() throws Exception {
        try (JunitSocketClient client = new JunitSocketClient(dmapServerPort, err)) {
            client.verify("ok DMAP");
            client.sendAndVerify("login trillian WRONGPW", "error");
            client.sendAndVerify("quit", "ok bye");
        }
    }

    @Test(timeout = 15000)
    public void acceptDmtpMessage_listDmapMessage() throws Exception {

        // accept a message via DMTP (to trillian)
        try (JunitSocketClient client = new JunitSocketClient(dmtpServerPort, err)) {
            client.verify("ok DMTP");
            client.sendAndVerify("begin", "ok");
            client.sendAndVerify("from arthur@earth.planet", "ok");
            client.sendAndVerify("to trillian@earth.planet", "ok 1");
            client.sendAndVerify("subject hello", "ok");
            client.sendAndVerify("data hello from junit", "ok");
            client.sendAndVerify("send", "ok");
            client.sendAndVerify("quit", "ok bye");
        }

        // list the message via DMAP list
        try (JunitSocketClient client = new JunitSocketClient(dmapServerPort, err)) {
            client.verify("ok DMAP");
            client.sendAndVerify("login trillian 12345", "ok");

            client.send("list");
            String listResult = client.listen();
            err.checkThat(listResult, containsString("arthur@earth.planet hello"));

            client.sendAndVerify("logout", "ok");
            client.sendAndVerify("quit", "ok bye");
        }
    }

    @Test(timeout = 15000)
    public void dmtpMessage_withUnknownRecipient_returnsError() throws Exception {

        // accept a message via DMTP (to trillian)
        try (JunitSocketClient client = new JunitSocketClient(dmtpServerPort, err)) {
            client.verify("ok DMTP");
            client.sendAndVerify("begin", "ok");
            client.sendAndVerify("from arthur@earth.planet", "ok");
            client.sendAndVerify("to unknown@earth.planet", "error unknown");
            client.sendAndVerify("quit", "ok bye");
        }
    }

}
