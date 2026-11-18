package dslab.transfer;

import dslab.nameserver.INameserverRemote;
import dslab.util.worker.ITCPWorkerFactory;
import dslab.util.worker.abstracts.DMTP2Worker;
import dslab.util.worker.abstracts.Worker;
import dslab.util.worker.handlers.IDMTP2Handler;

import java.net.InetAddress;
import java.net.Socket;

public class TransferDMTPWorkerFactory implements ITCPWorkerFactory {

    private final ForwardService forwardService;
    private final InetAddress inetAddress;
    private final INameserverRemote rootNs;

    public TransferDMTPWorkerFactory(ForwardService forwardService, InetAddress inetAddress, INameserverRemote rootNs) {
        this.forwardService = forwardService;
        this.inetAddress = inetAddress;
        this.rootNs = rootNs;
    }

    @Override
    public Worker newWorker(Socket socket) {
        IDMTP2Handler dmtpHandler = new TransferDMTP2Handler(forwardService, inetAddress, rootNs);
        return new DMTP2Worker(socket, dmtpHandler) {
        };
    }
}
