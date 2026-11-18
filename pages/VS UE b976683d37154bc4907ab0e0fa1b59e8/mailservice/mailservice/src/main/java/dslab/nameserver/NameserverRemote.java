package dslab.nameserver;

import dslab.util.Logger;

import java.rmi.RemoteException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class NameserverRemote implements INameserverRemote {

    private final ConcurrentMap<String, INameserverRemote> children = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, String> addresses = new ConcurrentHashMap<>();

    private final Logger logger;

    public NameserverRemote(Logger logger) {
        this.logger = logger;
    }

    @Override
    public void registerNameserver(String domain, INameserverRemote nameserver) throws RemoteException, AlreadyRegisteredException, InvalidDomainException {
        //TODO: domain syntax check

        String[] split = domain.split("\\.");

        if (split.length == 1) {
            logger.println("Registering nameserver for zone '%s'", domain);
            INameserverRemote result = children.computeIfAbsent(domain, e -> nameserver);

            if (result != nameserver) {
                logger.println("WARNING: The domain '%s' is already registerd", domain);
                throw new AlreadyRegisteredException("The domain " + domain + " is already registerd");
            }
        } else {
            String lookup = split[split.length - 1];

            INameserverRemote child = children.get(lookup);
            if (child == null)
                throw new InvalidDomainException("Could not find domain registry in child set" + lookup + " of given domain " + domain);

            String subdomain = substractSubdomain(split);

            logger.println("Forward nameserver registration request of '%s' to zone '%s'", domain, lookup);
            child.registerNameserver(subdomain, nameserver);
        }
    }

    @Override
    public void registerMailboxServer(String domain, String address) throws RemoteException, AlreadyRegisteredException, InvalidDomainException {

        String[] split = domain.split("\\.");

        if (split.length == 1) {
            logger.println("Registering mailbox server '%s' with address '%s'", domain, address);
            String result = addresses.computeIfAbsent(domain, e -> address);

            if (result != address) {
                logger.println("WARNING: Domain '%s' already registered with address '%s'", domain, result);
                throw new AlreadyRegisteredException(String.format("Domain '%s' already registered with address '%s'", domain, result));
            }
        } else {
            String lookup = split[split.length - 1];

            INameserverRemote child = children.get(lookup);
            if (child == null)
                throw new InvalidDomainException("Could not find domain registry in child set '" + lookup + "' of given domain " + domain);

            String subdomain = substractSubdomain(split);

            logger.println("Forward mailbox server registration request of '%s' to zone '%s'", domain, lookup);
            child.registerMailboxServer(subdomain, address);
        }

    }

    @Override
    public INameserverRemote getNameserver(String zone) throws RemoteException {
        logger.println("Nameserver for '%s' request by transfer server", zone);
        return children.get(zone);
    }

    @Override
    public String lookup(String username) throws RemoteException {
        logger.println("Address of '%s' request by transfer server", username);
        return addresses.get(username);
    }

    public Map<String, INameserverRemote> getChildren() {
        return new HashMap<>(children);
    }

    public Map<String, String> getAddresses() {
        return new HashMap<>(addresses);
    }


    private String substractSubdomain(String[] splittedDomain) {
        List<String> subdomain = new LinkedList<>(Arrays.asList(splittedDomain));
        subdomain.remove(subdomain.size() - 1);
        return String.join(".", subdomain);
    }
}
