package dslab.model;

import dslab.exception.DomainLookUpException;
import dslab.exception.ValidationException;
import dslab.nameserver.INameserverRemote;
import dslab.util.Config;

import java.rmi.RemoteException;
import java.util.*;

public class TransferSenderPreparation {

    private static final Config config = new Config("domains");
    private final INameserverRemote ns;
    private final List<ServerSpecificEmail> toSend = new ArrayList<ServerSpecificEmail>();
    private ServerSpecificEmail domainLookUpFailure;
    private boolean ignoreFailures = false;

    public TransferSenderPreparation(Email email, String ip, INameserverRemote ns) {
        this.ns = ns;
        domainLookUp(email, ip);
    }

    public static ServerSpecificEmail createEmailDeliveryFailure(String receiver, String data, String ip, INameserverRemote rooNs) throws DomainLookUpException {
        String failureDomain = getDomain(receiver);
        String[] ipPortFailure = getDomainAddress(failureDomain, rooNs);

        Email failureEmail = new Email();
        failureEmail.setSubject("Mail Delivery Error");
        failureEmail.setData(data);
        try {
            failureEmail.setFrom("mailer@" + ip);
            failureEmail.setRecipients(new ArrayList<String>(Collections.singleton(receiver)));
        } catch (ValidationException ignored) {
        }
        int port = Integer.parseInt(ipPortFailure[1]);
        ServerSpecificEmail mail = new ServerSpecificEmail(failureEmail, ipPortFailure[0], port);
        mail.setFailureMail(true);
        return mail;
    }

    public static String getDomain(String email) {
        return email.split("@")[1];
    }

    public static String[] getDomainAddress(String domain, INameserverRemote rootNs) throws DomainLookUpException {
        try {

            String[] domainSplit = domain.split("\\.");

            INameserverRemote next = rootNs;
            for (int i = domainSplit.length - 1; i > 0; --i) {
                next = next.getNameserver(domainSplit[i]);
                if (next == null) throw new DomainLookUpException();
            }
            String address = next.lookup(domainSplit[0]);
            if (address == null) throw new DomainLookUpException();

            return address.split(":");
        } catch (RemoteException e) {
            throw new RuntimeException("Lookup failed: " + e.getMessage(), e);
        }
    }

    public ServerSpecificEmail getDomainLookUpFailure() {
        return domainLookUpFailure;
    }

    public List<ServerSpecificEmail> getToSend() {
        return toSend;
    }

    private void domainLookUp(Email email, String ip) {

        List<String> recipients = new ArrayList<>(email.getRecipients());
        email = new Email(email);
        email.getRecipients().clear();

        HashMap<String, ServerSpecificEmail> emailsToSend = new HashMap<String, ServerSpecificEmail>();

        for (String r : recipients) {
            String domain = getDomain(r);

            String[] ipPort;
            try {
                if (!emailsToSend.containsKey(domain)) {
                    ipPort = getDomainAddress(domain, ns);
                    emailsToSend.put(domain, new ServerSpecificEmail(email, ipPort[0], Integer.parseInt(ipPort[1])));
                }

                emailsToSend.get(domain).getRecipients().add(r);

            } catch (DomainLookUpException e) {
                if (ignoreFailures) continue;

                if (domainLookUpFailure == null) {
                    try {
                        domainLookUpFailure = createEmailDeliveryFailure(email.getFrom(), "error domain unknown for ", ip, ns);
                        domainLookUpFailure.setFailureMail(true);
                    } catch (DomainLookUpException ex) {
                        ignoreFailures = true;
                        continue;
                    }
                }

                domainLookUpFailure.setData(domainLookUpFailure.getData() + r + ",");
            }
        }

        for (Map.Entry<String, ServerSpecificEmail> entry : emailsToSend.entrySet()) {
            toSend.add(entry.getValue());
        }
    }


}
