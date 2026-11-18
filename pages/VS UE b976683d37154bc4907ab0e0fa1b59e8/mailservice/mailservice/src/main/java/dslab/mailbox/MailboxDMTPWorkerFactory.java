package dslab.mailbox;

import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.util.Config;
import dslab.util.worker.ITCPWorkerFactory;
import dslab.util.worker.abstracts.DMTP2Worker;
import dslab.util.worker.abstracts.Worker;
import dslab.util.worker.handlers.IDMTP2Handler;

import java.net.Socket;

public class MailboxDMTPWorkerFactory implements ITCPWorkerFactory {
    private final Config config;
    private final IMailboxDataRepository repo;

    public MailboxDMTPWorkerFactory(Config config, IMailboxDataRepository repo) {
        this.config = config;
        this.repo = repo;
    }

    @Override
    public Worker newWorker(Socket socket) {
        IDMTP2Handler dmtp2Handler = new MailboxDMTP2Handler(config, repo);
        return new DMTP2Worker(socket, dmtp2Handler) {
        };
    }
}
