package dslab.transfer;

import dslab.exception.ValidationException;
import dslab.model.ServerSpecificEmail;
import dslab.model.TransferSenderPreparation;
import dslab.nameserver.INameserverRemote;
import dslab.util.worker.abstracts.DMTPHandler;

import java.net.InetAddress;
import java.util.List;

public class TransferDMTPHandler extends DMTPHandler {

    private final InetAddress serverAdress;
    private final ForwardService forwardService;
    private final INameserverRemote rootNs;

    public TransferDMTPHandler(ForwardService forwardService, InetAddress serverAdress, INameserverRemote rootNs) {
        this.forwardService = forwardService;
        this.serverAdress = serverAdress;
        this.rootNs = rootNs;
    }

    @Override
    public String send() throws ValidationException {
        getEmail().valid();

        // send to mailbox server
        TransferSenderPreparation transferSenderPreparation = new TransferSenderPreparation(getEmail(), serverAdress.getHostAddress(), rootNs);

        // send valid emails
        List<ServerSpecificEmail> toSend = transferSenderPreparation.getToSend();
        toSend.forEach(forwardService::forward);

        // send error of domain lookup failures
        ServerSpecificEmail lookUpFailures = transferSenderPreparation.getDomainLookUpFailure();

        if (lookUpFailures != null) forwardService.forward(lookUpFailures);

        return "ok";
    }
}
