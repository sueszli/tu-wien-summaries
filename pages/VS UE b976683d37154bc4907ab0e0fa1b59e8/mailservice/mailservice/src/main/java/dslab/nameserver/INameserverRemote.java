package dslab.nameserver;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;

/**
 * The remote object of a nameserver which can be called via RMI.
 */
public interface INameserverRemote extends Remote {

    /**
     * Registers a mailbox server with the given address for the given domain. For example, when registering a
     * nameserver for the domain 'earth.planet', the new nameserver first calls the root nameserver with the argument
     * 'earth.planet'. The root nameserver locates the nameserver for 'planet' via its child-nameserver references, and
     * invokes this method with the remainder of the domain (i.e., 'earth'). Because 'earth' is then the leaf zone, the
     * current nameserver ('planet') stores the reference in its child-nameserver references.
     *
     * @param domain     the domain
     * @param nameserver the nameserver's remote object
     * @throws RemoteException            RMI exception (declaration required by RMI)
     * @throws AlreadyRegisteredException if the given domain is already registered
     * @throws InvalidDomainException     if the domain is invalid (e.g., due to a syntax error, or a required intermediary
     *                                    nameserver was not found)
     */
    void registerNameserver(String domain, INameserverRemote nameserver) throws RemoteException, AlreadyRegisteredException, InvalidDomainException;

    /**
     * Registers a mailbox server with the given address for the given domain.
     *
     * @param domain  the mail domain, e.g. <code>vienna.earth.planet</code>
     * @param address the socket address of the mailbox server's DMTP socket, e.g., <code>127.0.0.1:16503</code>
     * @throws RemoteException            RMI exception (declaration required by RMI)
     * @throws AlreadyRegisteredException if the given domain is already in use
     * @throws InvalidDomainException     if the domain is invalid (e.g., due to a syntax error, or the responsible
     *                                    nameserver was not found)
     */
    void registerMailboxServer(String domain, String address) throws RemoteException, AlreadyRegisteredException, InvalidDomainException;

    /**
     * Returns a reference to the remote object of the ns for the given zone. For example, if called with the argument
     * 'earth' on the remote object of zone 'planet', the call returns the reference to the nameserver of the zone
     * 'earth.planet'.
     *
     * @param zone the child zone, e.g. <code>earth</code>
     * @return the remote object reference of the given zone, or <code>null</code> if it does not exist
     * @throws RemoteException RMI exception (declaration required by RMI)
     */
    INameserverRemote getNameserver(String zone) throws RemoteException;

    /**
     * @param username
     * @return
     * @throws RemoteException
     */
    String lookup(String username) throws RemoteException;

    Map<String, INameserverRemote> getChildren() throws RemoteException;

    Map<String, String> getAddresses() throws RemoteException;


}
