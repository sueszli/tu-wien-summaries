package dslab.util.worker.abstracts;

import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.util.protocolParser.listener.DMTP2Listener;
import dslab.util.worker.handlers.IDMTP2Handler;

import java.net.Socket;

public class DMTP2Worker extends DMTPWorker implements DMTP2Listener {

    private final IDMTP2Handler dmtpHandler;

    public DMTP2Worker(Socket clientSocket, IDMTP2Handler dmtpHandler) {
        super(clientSocket, dmtpHandler, "ok DMTP2.0");
        this.dmtpHandler = dmtpHandler;
    }

    @Override
    @Command
    public String hash(String hash) {
        return dmtpHandler.hash(hash);
    }
}
