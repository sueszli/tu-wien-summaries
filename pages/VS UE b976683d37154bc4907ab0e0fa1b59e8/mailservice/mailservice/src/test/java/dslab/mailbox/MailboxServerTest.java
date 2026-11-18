package dslab.mailbox;

import dslab.ComponentFactory;
import dslab.Constants;
import dslab.Sockets;
import dslab.TestBase;
import dslab.monitoring.MonitoringServerTest;
import dslab.nsHelper.NsSetupHelper;
import dslab.nsHelper.NsSetupHelperFactory;
import dslab.util.Config;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.SocketTimeoutException;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * MailboxServerTest.
 */
public class MailboxServerTest extends TestBase {

    private static final Log LOG = LogFactory.getLog(MonitoringServerTest.class);

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

    @Test
    public void runAndShutdownTransferServer_createsAndStopsTcpSocketCorrectly() throws Exception {
        String componentId = "mailbox-earth-planet";
        Config config = new Config(componentId);

        IMailboxServer component = ComponentFactory.createMailboxServer(componentId, in, out);
        int dmtpPort = config.getInt("dmtp.tcp.port");
        int dmapPort = config.getInt("dmap.tcp.port");

        assertThat(component, is(notNullValue()));

        Thread componentThread = new Thread(component);
        LOG.info("Starting thread with component " + component);
        componentThread.start();

        try {
            LOG.info("Waiting for DMTP socket to open on port " + dmtpPort);
            Sockets.waitForSocket("localhost", dmtpPort, Constants.COMPONENT_STARTUP_WAIT);
        } catch (SocketTimeoutException e) {
            err.addError(new AssertionError("Expected a TCP server socket on port " + dmtpPort, e));
        }

        try {
            LOG.info("Waiting for DMAP socket to open on port " + dmapPort);
            Sockets.waitForSocket("localhost", dmapPort, Constants.COMPONENT_STARTUP_WAIT);
        } catch (SocketTimeoutException e) {
            err.addError(new AssertionError("Expected a TCP server socket on port " + dmapPort, e));
        }

        LOG.info("Shutting down component " + component);
        in.addLine("shutdown"); // send "shutdown" command to command line
        Thread.sleep(Constants.COMPONENT_TEARDOWN_WAIT);

        try {
            LOG.info("Waiting for thread to stop for component " + component);
            componentThread.join();
        } catch (InterruptedException e) {
            err.addError(new AssertionError("Monitoring server was not terminated correctly"));
        }


        err.checkThat("Expected tcp socket on port " + dmtpPort + " to be closed after shutdown",
                Sockets.isServerSocketOpen(dmtpPort), is(false));

        err.checkThat("Expected tcp socket on port " + dmapPort + " to be closed after shutdown",
                Sockets.isServerSocketOpen(dmapPort), is(false));
    }

}
