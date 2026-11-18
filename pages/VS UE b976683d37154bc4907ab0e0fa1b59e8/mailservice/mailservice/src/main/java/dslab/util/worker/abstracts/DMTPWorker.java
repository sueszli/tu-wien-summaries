package dslab.util.worker.abstracts;

import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.exception.ValidationException;
import dslab.util.protocolParser.listener.DMTPListener;
import dslab.util.worker.handlers.IDMTPHandler;

import java.net.Socket;
import java.util.List;

public abstract class DMTPWorker extends TCPWorker implements DMTPListener {

    private final IDMTPHandler dmtpHandler;

    public DMTPWorker(Socket clientSocket, IDMTPHandler dmtpHandler) {
        super(clientSocket, "ok DMTP");
        this.dmtpHandler = dmtpHandler;
    }

    public DMTPWorker(Socket clientSocket, IDMTPHandler dmtpHandler, String initMessage) {
        super(clientSocket, initMessage);
        this.dmtpHandler = dmtpHandler;
    }

    @Override
    @Command
    public String begin() {
        return dmtpHandler.begin();
    }

    @Override
    @Command
    public String subject(List<String> subject) {
        return dmtpHandler.subject(subject);
    }

    @Override
    @Command
    public String data(List<String> data) {
        return dmtpHandler.data(data);
    }

    @Override
    @Command
    public String from(String from) throws ValidationException {
        return dmtpHandler.from(from);
    }

    @Override
    @Command
    public String to(String to) throws ValidationException {
        return dmtpHandler.to(to);
    }

    @Override
    @Command
    public String send() throws ValidationException {
        return dmtpHandler.send();
    }


}
