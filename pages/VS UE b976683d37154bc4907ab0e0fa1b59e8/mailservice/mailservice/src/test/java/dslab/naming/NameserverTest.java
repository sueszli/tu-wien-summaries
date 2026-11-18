package dslab.naming;

import dslab.ComponentFactory;
import dslab.Constants;
import dslab.TestInputStream;
import dslab.TestOutputStream;
import dslab.nameserver.INameserverRemote;
import dslab.util.Config;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ErrorCollector;
import org.junit.runners.MethodSorters;

import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import static org.hamcrest.CoreMatchers.*;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class NameserverTest {

    private static final Log LOG = LogFactory.getLog(NameserverTest.class);

    @Rule
    public ErrorCollector err = new ErrorCollector();

    private TestInputStream nsRootIn;
    private TestOutputStream nsRootOut;

    private Config nsPlanetConfig;
    private TestInputStream nsPlanetIn;
    private TestOutputStream nsPlanetOut;

    private static void sendShutdown(TestInputStream in) {
        try {
            LOG.info("Sending shutdown to component component");
            in.addLine("shutdown");
            Thread.sleep(Constants.COMPONENT_TEARDOWN_WAIT);
        } catch (InterruptedException e) {
            LOG.error("Interrupted while waiting on component teardown");
            throw new RuntimeException(e);
        }
    }

    @Before
    public void setUp() throws Exception {
        this.nsRootIn = new TestInputStream();
        this.nsPlanetIn = new TestInputStream();
        this.nsRootOut = new TestOutputStream();
        this.nsPlanetOut = new TestOutputStream();

        this.nsPlanetConfig = new Config("ns-planet");
    }

    @Test(timeout = 15000)
    public void ns01_startAndShutdownRootNameserver_createsAndUnexportsRegistryCorrectly() throws Exception {
        Runnable nsRoot = ComponentFactory.createNameserver("ns-root", nsRootIn, nsRootOut);

        LOG.info("Starting ns-root thread");
        Thread nsRootThread = new Thread(nsRoot);
        nsRootThread.start();

        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);
        try {
            Registry registry = LocateRegistry.getRegistry(nsPlanetConfig.getString("registry.host"), nsPlanetConfig.getInt("registry.port"));
            registry.list();
        } catch (RemoteException e) {
            err.addError(new AssertionError("Expected root nameserver to start a registry, but it couldn't be located or registry.list() failed", e));
        } finally {
            sendShutdown(nsRootIn);

            try {
                Registry registry = LocateRegistry.getRegistry(nsPlanetConfig.getString("registry.host"), nsPlanetConfig.getInt("registry.port"));
                registry.list();
                err.addError(new AssertionError("Expected root nameserver to shut down the registry, but could call list"));
            } catch (RemoteException e) {
                // ok
            } catch (Exception e) {
                err.addError(e);
            }
        }

    }

    @Test(timeout = 15000)
    public void ns02_startRootNameserver_registersRemoteObjectCorrectly() throws Exception {
        Runnable nsRoot = ComponentFactory.createNameserver("ns-root", nsRootIn, nsRootOut);

        LOG.info("Starting ns-root thread");
        Thread nsRootThread = new Thread(nsRoot);
        nsRootThread.start();

        String id = nsPlanetConfig.getString("root_id");
        try {
            Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);
            Registry registry = LocateRegistry.getRegistry(nsPlanetConfig.getString("registry.host"), nsPlanetConfig.getInt("registry.port"));

            LOG.info("Looking up " + id + " in registry");
            Remote remote = registry.lookup(id);
            err.checkThat("Remote object bound to " + id + " should be a INameserverRemote", remote, instanceOf(INameserverRemote.class));

            String[] list = registry.list();
            err.checkThat("Registry should only contain a single remote object (the root nameserver)", list.length, is(1));
        } catch (RemoteException | NotBoundException e) {
            err.addError(new AssertionError("Error while looking up remote object " + ""));
        } finally {
            sendShutdown(nsRootIn);
        }
    }

    @Test(timeout = 30000)
    public void ns03_startingZoneNameserver_registersNameserverCorrectly() throws Exception {
        Runnable nsRoot = ComponentFactory.createNameserver("ns-root", nsRootIn, nsRootOut);

        LOG.info("Starting ns-root thread");
        Thread nsRootThread = new Thread(nsRoot);
        nsRootThread.start();
        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);

        LOG.info("Starting ns-planet thread");
        Runnable nsPlanet = ComponentFactory.createNameserver("ns-planet", nsPlanetIn, nsPlanetOut);
        Thread nsPlanetThread = new Thread(nsPlanet);
        nsPlanetThread.start();
        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);

        try {
            // lookup registry and root remote
            Registry registry = LocateRegistry.getRegistry(nsPlanetConfig.getString("registry.host"), nsPlanetConfig.getInt("registry.port"));

            err.checkThat("Adding a new nameserver should not bind an additional remote object to the registry", registry.list().length, is(1));

            String id = nsPlanetConfig.getString("root_id");
            LOG.info("Looking up " + id + " in registry");
            INameserverRemote root = (INameserverRemote) registry.lookup(id); // from ns02 we assume that this has the correct type

            // check the nameserver remote
            LOG.info("Getting nameserver 'planet' from server remote");
            Remote zone = root.getNameserver("planet");

            err.checkThat(zone, not(nullValue()));

            // read nameservers from ns-root's command line
            LOG.info("Sending 'nameservers' command");
            nsRootIn.addLine("nameservers");
            String output = nsRootOut.listen();
            LOG.info("Got output: " + output);
            err.checkThat("output of 'nameservers' command should contain the domain", output, containsString("planet"));
        } finally {
            // shutdown
            sendShutdown(nsPlanetIn);
            sendShutdown(nsRootIn);
        }
    }

    @Test(timeout = 30000)
    public void ns04_registerAndLookupMailboxServer_registersAndReturnsAddressCorrectly() throws Exception {
        Runnable nsRoot = ComponentFactory.createNameserver("ns-root", nsRootIn, nsRootOut);

        LOG.info("Starting ns-root thread");
        Thread nsRootThread = new Thread(nsRoot);
        nsRootThread.start();
        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);

        LOG.info("Starting ns-planet thread");
        Runnable nsPlanet = ComponentFactory.createNameserver("ns-planet", nsPlanetIn, nsPlanetOut);
        Thread nsPlanetThread = new Thread(nsPlanet);
        nsPlanetThread.start();
        Thread.sleep(Constants.COMPONENT_STARTUP_WAIT);

        try {
            // lookup registry and root remote
            Registry registry = LocateRegistry.getRegistry(nsPlanetConfig.getString("registry.host"), nsPlanetConfig.getInt("registry.port"));

            String id = nsPlanetConfig.getString("root_id");
            LOG.info("Looking up " + id + " in registry");
            INameserverRemote root = (INameserverRemote) registry.lookup(id); // from ns02 we assume that this has the correct type

            // registration
            LOG.info("Registering mailbox server for @mars.planet mail domain");
            root.registerMailboxServer("mars.planet", "192.168.0.1:14242");

            Thread.sleep(1000);

            INameserverRemote zone = root.getNameserver("planet");
            LOG.info("Looking up 'mars' at nameserver planet");
            String address = zone.lookup("mars");

            err.checkThat("registered address should contain both address and port", address, allOf(containsString("192.168.0.1"), containsString("14242")));

        } finally {
            // shutdown
            sendShutdown(nsPlanetIn);
            sendShutdown(nsRootIn);
        }
    }
}
