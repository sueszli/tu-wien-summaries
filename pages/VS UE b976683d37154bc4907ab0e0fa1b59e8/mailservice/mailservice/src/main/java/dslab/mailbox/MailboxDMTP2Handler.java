package dslab.mailbox;

import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.util.Config;
import dslab.util.worker.handlers.IDMTP2Handler;

public class MailboxDMTP2Handler extends MailboxDMTPHandler implements IDMTP2Handler {

    public MailboxDMTP2Handler(Config config, IMailboxDataRepository dataRepository) {
        super(config, dataRepository);
    }

    @Override
    public String hash(String hash) {
        validateBegin();
        getEmail().setHash(hash);
        return "ok";
    }
}
