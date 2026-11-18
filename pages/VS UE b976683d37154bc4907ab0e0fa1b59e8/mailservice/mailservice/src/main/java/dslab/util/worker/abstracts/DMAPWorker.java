package dslab.util.worker.abstracts;

import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.exception.ValidationException;
import dslab.util.protocolParser.listener.DMAPListener;
import dslab.util.worker.handlers.IDMAPHandler;

import java.net.Socket;

public abstract class DMAPWorker extends TCPWorker implements DMAPListener {


    private final IDMAPHandler dmapHandler;

    protected DMAPWorker(Socket clientSocket, IDMAPHandler dmapHandler) {
        super(clientSocket, "ok DMAP");

        this.dmapHandler = dmapHandler;
    }

    protected DMAPWorker(Socket clientSocket, IDMAPHandler dmapHandler, String initMessage) {
        super(clientSocket, initMessage);

        this.dmapHandler = dmapHandler;
    }

    @Override
    @Command
    public String login(String username, String password) throws ValidationException {
        return dmapHandler.login(username, password);
    }

    @Override
    @Command
    public String list() throws ValidationException {
        return dmapHandler.list();
    }

    @Override
    @Command
    public String logout() throws ValidationException {
        return dmapHandler.logout();
    }

    @Override
    @Command
    public String show(Integer emailId) throws ValidationException {
        return dmapHandler.show(emailId);
    }

    @Override
    @Command
    public String delete(Integer emailId) throws ValidationException {
        return dmapHandler.delete(emailId);
    }
}
