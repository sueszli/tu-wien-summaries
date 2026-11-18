package dslab.nameserver;

import at.ac.tuwien.dsg.orvell.Shell;
import at.ac.tuwien.dsg.orvell.StopShellException;
import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.ComponentFactory;
import dslab.util.Config;
import dslab.util.Logger;

import java.io.InputStream;
import java.io.PrintStream;
import java.rmi.AlreadyBoundException;
import java.rmi.NoSuchObjectException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;
import java.util.Set;

public class Nameserver implements INameserver {

    private final Config config;
    private final Shell shell;

    private final boolean isRoot;
    private final INameserverRemote nsObj; //to export und unexport
    private Registry registry;
    private INameserverRemote nsRemote; // available to invoke remotly


    /**
     * Creates a new server instance.
     *
     * @param componentId the id of the component that corresponds to the Config resource
     * @param config      the component config
     * @param in          the input stream to read console input from
     * @param out         the output stream to write console output to
     */
    public Nameserver(String componentId, Config config, InputStream in, PrintStream out) {
        // TODO
        this.config = config;
        isRoot = !config.containsKey("domain");

        shell = new Shell(in, out);
        shell.register(this);
        shell.setPrompt(componentId + "> ");

        this.nsObj = new NameserverRemote(new Logger(shell.out()));
    }

    public static void main(String[] args) throws Exception {
        INameserver component = ComponentFactory.createNameserver(args[0], System.in, System.out);
        component.run();
    }

    @Override
    public void run() {


        try {
            if (isRoot) runRoot();
            else runZone();
        } catch (AlreadyBoundException e) {//TODO: meaningful exception handling
            close();
            throw new RuntimeException("[ERROR] coudl not create remote object: " + e.getMessage(), e);
        } catch (RemoteException | AlreadyRegisteredException | InvalidDomainException |
                 NotBoundException e) { //TODO: meaningful exception handling
            close();
            throw new RuntimeException("[ERROR] Could not start rootserver: " + e.getMessage(), e);
        }

        shell.run();

    }

    @Override
    @Command
    public void nameservers() {
        try {
            Set<String> names = nsRemote.getChildren().keySet();

            StringBuilder result = new StringBuilder();
            int num = 1;
            for (String n : names) {
                result.append(String.format("%d. %s\n", num++, n));
            }

            shell.out().println(result);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    @Override
    @Command
    public void addresses() {
        try {
            Set<Map.Entry<String, String>> addresses = nsRemote.getAddresses().entrySet();
            StringBuilder result = new StringBuilder();
            int num = 1;
            for (Map.Entry<String, String> a : addresses) {
                result.append(String.format("%d. %s %s\n", num++, a.getKey(), a.getValue()));
            }
            shell.out().println(result);

        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    @Override
    @Command
    public void shutdown() {
        // TODO
        close();
        throw new StopShellException();
    }

    private void close() {
        try {
            UnicastRemoteObject.unexportObject(nsObj, false); //TODO:Check force
        } catch (NoSuchObjectException e) {
            e.printStackTrace();
        }

        if (isRoot) {
            try {
                registry.unbind(config.getString("root_id"));
                UnicastRemoteObject.unexportObject(registry, false);
            } catch (RemoteException | NotBoundException e) {
                e.printStackTrace();
            }
        }
    }

    private void runRoot() throws RemoteException, AlreadyBoundException {
        registry = LocateRegistry.createRegistry(config.getInt("registry.port"));
        nsRemote = (INameserverRemote) UnicastRemoteObject.exportObject(nsObj, 0);
        registry.bind(config.getString("root_id"), nsRemote);
    }

    private void runZone() throws RemoteException, NotBoundException, AlreadyRegisteredException, InvalidDomainException {
        nsRemote = (INameserverRemote) UnicastRemoteObject.exportObject(nsObj, 0);
        Registry registry = LocateRegistry.getRegistry(config.getString("registry.host"), config.getInt("registry.port"));

        INameserverRemote root = (INameserverRemote) registry.lookup(config.getString("root_id"));
        root.registerNameserver(config.getString("domain"), nsRemote);
    }

}
