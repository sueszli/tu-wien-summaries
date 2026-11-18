package dslab.model;

public class ServerSpecificEmail extends Email {

    private final String mailboxIp;
    private final int mailboxPort;

    private boolean isFailureMail = false;

    ServerSpecificEmail(Email email, String ip, int port) {
        super(email);

        this.mailboxIp = ip;
        this.mailboxPort = port;
    }

    public String getMailboxIp() {
        return mailboxIp;
    }

    public int getMailboxPort() {
        return mailboxPort;
    }

    public boolean isFailureMail() {
        return isFailureMail;
    }

    public void setFailureMail(boolean failureMail) {
        isFailureMail = failureMail;
    }

    @Override
    public String toString() {
        return "ServerSpecificEmail{" + "mailboxIp='" + mailboxIp + '\'' + ", mailboxPort=" + mailboxPort + "} " + super.toString();
    }
}
