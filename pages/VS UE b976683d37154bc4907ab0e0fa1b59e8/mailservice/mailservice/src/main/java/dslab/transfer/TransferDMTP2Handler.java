package dslab.transfer;

import dslab.nameserver.INameserverRemote;
import dslab.util.worker.handlers.IDMTP2Handler;

import java.net.InetAddress;

public class TransferDMTP2Handler extends TransferDMTPHandler implements IDMTP2Handler {

    public TransferDMTP2Handler(ForwardService forwardService, InetAddress serverAdress, INameserverRemote rootNs) {
        super(forwardService, serverAdress, rootNs);
    }

    @Override
    public String hash(String hash) {
        validateBegin();
        getEmail().setHash(hash);
        return "ok";
    }
}
