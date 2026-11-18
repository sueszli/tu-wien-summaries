package dslab.monitoring;

import dslab.ComponentFactory;
import dslab.Constants;
import dslab.Sockets;
import dslab.TestBase;
import dslab.util.Config;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Tests the creation, running, and shutting down of the monitoring server.
 */
public class MonitoringServerTest extends TestBase {

    private static final Log LOG = LogFactory.getLog(MonitoringServerTest.class);

    @Test
    public void runAndShutdownMonitoringServer_createsAndStopsUdpSocketCorrectly() throws Exception {
        IMonitoringServer component = ComponentFactory.createMonitoringServer("monitoring", in, out);
        int port = new Config("monitoring").getInt("udp.port");

        assertThat(component, is(notNullValue()));

        Thread componentThread = new Thread(component);
        LOG.info("Starting thread with component " + component);
        componentThread.start();

        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT); // wait a bit for resources to be initialized

        try {
            LOG.info("Trying to create socket on port " + port);
            err.checkThat("Expected an open UDP socket on port " + port, Sockets.isDatagramSocketOpen(port), is(true));
        } catch (Exception e) {
            // a different unexpected error occurred (unlikely)
            err.addError(e);
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

        err.checkThat("Expected datagram socket on port " + port + " to be closed after shutdown",
                Sockets.isDatagramSocketOpen(port), is(false));
    }

}
