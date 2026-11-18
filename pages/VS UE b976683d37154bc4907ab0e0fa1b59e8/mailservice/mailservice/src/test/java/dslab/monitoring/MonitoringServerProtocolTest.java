package dslab.monitoring;

import dslab.ComponentFactory;
import dslab.Constants;
import dslab.TestBase;
import dslab.util.Config;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;

/**
 * Tests whether the UDP-based monitoring protocol is implemented correctly on the server side.
 */
public class MonitoringServerProtocolTest extends TestBase {

    private static final Log LOG = LogFactory.getLog(MonitoringServerProtocolTest.class);

    private String componentId = "monitoring";

    private IMonitoringServer component;
    private InetSocketAddress addr;

    @Before
    public void setUp() throws Exception {
        component = ComponentFactory.createMonitoringServer(componentId, in, out);
        addr = new InetSocketAddress("127.0.0.1", new Config(componentId).getInt("udp.port"));

        new Thread(component).start();
        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);
    }

    @After
    public void tearDown() throws Exception {
        in.addLine("shutdown");
        Thread.sleep(Constants.COMPONENT_TEARDOWN_WAIT);
    }

    @Test(timeout = 15000)
    public void addresses_returnsCorrectStatistics() throws Exception {
        LOG.info("Sending three monitoring packets to monitoring socket");
        try (DatagramSocket socket = new DatagramSocket()) {
            String str1 = "127.0.0.1:42 foo@example.com";
            String str2 = "127.0.0.1:43 foo@example.com";
            String str3 = "127.0.0.1:42 bar@example.com";

            socket.send(new DatagramPacket(str1.getBytes(), str1.length(), addr));
            socket.send(new DatagramPacket(str2.getBytes(), str2.length(), addr));
            socket.send(new DatagramPacket(str3.getBytes(), str3.length(), addr));
        }

        Thread.sleep(2500);
        in.addLine("addresses"); // send "addresses" command to command line
        Thread.sleep(2500);
        String output = String.join(",", out.getLines());
        assertThat(output, containsString("foo@example.com 2"));
        assertThat(output, containsString("bar@example.com 1"));
    }

    /*
     * Assumes that run and shutdown works correctly.
     */
    @Test(timeout = 15000)
    public void servers_returnsCorrectStatistics() throws Exception {
        LOG.info("Sending three monitoring packets to monitoring socket");
        try (DatagramSocket socket = new DatagramSocket()) {
            String str1 = "127.0.0.1:42 foo@example.com";
            String str2 = "127.0.0.1:43 foo@example.com";
            String str3 = "127.0.0.1:42 bar@example.com";

            socket.send(new DatagramPacket(str1.getBytes(), str1.length(), addr));
            socket.send(new DatagramPacket(str2.getBytes(), str2.length(), addr));
            socket.send(new DatagramPacket(str3.getBytes(), str3.length(), addr));
        }

        Thread.sleep(2500);
        in.addLine("servers"); // send "addresses" command to command line
        Thread.sleep(2500);
        String output = String.join(",", out.getLines());
        assertThat(output, containsString("127.0.0.1:42 2"));
        assertThat(output, containsString("127.0.0.1:43 1"));
    }
}
