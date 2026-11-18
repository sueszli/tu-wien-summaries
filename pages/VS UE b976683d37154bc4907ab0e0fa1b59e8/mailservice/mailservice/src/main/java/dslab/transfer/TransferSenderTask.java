package dslab.transfer;

import dslab.exception.DomainLookUpException;
import dslab.exception.ErrorResponseException;
import dslab.exception.NoOkResponseException;
import dslab.model.ServerSpecificEmail;
import dslab.model.TransferSenderPreparation;
import dslab.nameserver.INameserverRemote;
import dslab.util.sockcom.SockCom;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class TransferSenderTask implements ITransferSenderTask {


    private final MonitoringService monitoringService;
    private final ServerSpecificEmail email;
    private final InetAddress inetAddress;
    private final INameserverRemote rootNs;
    private Socket socket;
    private SockCom comm;

    public TransferSenderTask(ServerSpecificEmail email, InetAddress inetAddress, MonitoringService monitoringService, INameserverRemote rootNs) {
        this.email = email;
        this.inetAddress = inetAddress;
        this.monitoringService = monitoringService;
        this.rootNs = rootNs;
    }

    @Override
    public void run() {
        try {
            socket = connectToServer();
            comm = new SockCom(socket);
            try {
                sendEmail();
            } catch (NoOkResponseException e) {
                if (!email.isFailureMail())
                    sendFailureMail("error sending mail " + String.join(",", email.getRecipients()));
            }

            comm.writeLine("quit");
            closeConnection();
        } catch (SocketException ignored) {
            System.out.println("Stop sending because of Socket is closed");
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public void sendEmail() throws IOException, NoOkResponseException {
//            System.out.println("Run email to " + email.getRecipients());

        try {
            comm.readLine();
            comm.writeLine("begin");
            comm.readLine();
            int attemptCounter = 0;
            do {
                comm.writeLine("to " + String.join(",", email.getRecipients()));
            } while (!checkToResponse() && !email.isFailureMail() && attemptCounter++ < 2 && !email.getRecipients().isEmpty());
            if (email.getRecipients().isEmpty()) return;

            comm.writeLine("data " + email.getData());
            comm.readLine();
            comm.writeLine("subject " + email.getSubject());
            comm.readLine();
            comm.writeLine("from " + email.getFrom());
            comm.readLine();
            if (email.getHash() != null) {
                comm.writeLine("hash " + email.getHash());
                comm.readLine();
            }
            comm.writeLine("send");
            checkOkResponse();
            sendUdpData(); // on successfull sending
        } catch (ErrorResponseException e) {
            throw new NoOkResponseException();
        }
    }

    Socket connectToServer() {
        try {
            return new Socket(email.getMailboxIp(), email.getMailboxPort());
        } catch (IOException e) {
            sendFailureMail("could not connect to recipient server");
            throw new RuntimeException("Could not connect to " + email.getMailboxIp() + ":" + email.getMailboxPort() + ": " + e.getMessage(), e);
        }
    }

    public void closeConnection() {
        if (socket != null && !socket.isClosed()) try {
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void checkOkResponse() throws NoOkResponseException, IOException, ErrorResponseException {
        String res = comm.readLine().trim();
        if (!res.startsWith("ok")) throw new NoOkResponseException();

    }

    private boolean checkToResponse() throws IOException {
        try {
            comm.readLine();
            return true;
        } catch (ErrorResponseException e) {
            if (email.isFailureMail()) return false;

            String res = e.getMessage();
            String[] tmp = res.split(" ");

            if (List.of(tmp).contains("unknown") && List.of(tmp).contains("recipient")) {
                List<String> invalidNames = Arrays.stream(tmp[tmp.length - 1].split(",")).map(String::trim).collect(Collectors.toList());

                List<String> invalidEmails = email.getRecipients().stream().filter(r -> invalidNames.contains(r.split("@")[0])).collect(Collectors.toList());

                email.getRecipients().removeAll(invalidEmails);

                String errorString = "error not existing emails " + String.join(",", invalidEmails);
                sendFailureMail(errorString);
            }
            return false;
        }
    }

    private void sendFailureMail(String error) {
        try {
            ServerSpecificEmail failureMail = TransferSenderPreparation.createEmailDeliveryFailure(email.getFrom(), error, inetAddress.getHostAddress(), rootNs);
            (new TransferSenderTask(failureMail, inetAddress, monitoringService, rootNs)).run();
        } catch (DomainLookUpException ignored) {
        }
    }

    private void sendUdpData() {
        String data = inetAddress.getHostAddress() + ":" + inetAddress.getHostAddress() + " " + this.email.getFrom();
        monitoringService.sendData(data);
    }


}
