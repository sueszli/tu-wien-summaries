package dslab.mailbox;

import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.util.Config;
import dslab.util.worker.ITCPWorkerFactory;
import dslab.util.worker.abstracts.Worker;
import dslab.util.worker.handlers.IDMAPHandler;

import java.net.Socket;
import java.security.PrivateKey;

public class MailboxDMAPWorkerFactory implements ITCPWorkerFactory {
    private final Config users;
    private final IMailboxDataRepository repo;
    private final PrivateKey key;
    private final String componentId;

    public MailboxDMAPWorkerFactory(Config users, IMailboxDataRepository repo, PrivateKey key, String componentId) {
        this.users = users;
        this.repo = repo;
        this.key = key;
        this.componentId = componentId;
    }

    @Override
    public Worker newWorker(Socket socket) {
        IDMAPHandler dmapHandler = new MailboxDMAP2Handler(users, repo);
        return new MailboxDMAP2Worker(socket, dmapHandler, componentId, key);
    }
}
