package dslab.nsHelper;

import dslab.ComponentFactory;
import dslab.Constants;
import dslab.TestInputStream;
import dslab.TestOutputStream;
import dslab.mailbox.MailboxServerProtocolTest;
import dslab.nameserver.INameserver;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class NsSetupModel {

    private static final Log LOG = LogFactory.getLog(MailboxServerProtocolTest.class);

    private final TestInputStream in = new TestInputStream();
    private final TestOutputStream out = new TestOutputStream();
    private final String componentId;

    private INameserver nameserver;
    private Thread runningThread;

    private boolean started;

    public NsSetupModel(String componentId) {
        this.componentId = componentId;
    }

    public TestOutputStream out() {
        return out;
    }

    public TestInputStream in() {
        return in;
    }

    public void start() throws Exception {
        LOG.info("Startup NS component " + componentId);
        nameserver = ComponentFactory.createNameserver(componentId, in, out);

        runningThread = new Thread(nameserver);
        runningThread.start();

        started = true;
        Thread.sleep(Constants.NS_COMPONENT_STARTUP_WAIT);
    }

    public boolean isStarted() {
        return started;
    }

    public void shutdown() {
        LOG.info("Shutdown NS component " + componentId);
        if (!started) return;

        in.addLine("shutdown");
        try {
            Thread.sleep(Constants.NS_COMPONENT_SHUTDOWN_WAIT);
        } catch (InterruptedException ignored) {
        }
    }
}
