package dslab.mailbox;

import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.util.Config;
import dslab.util.worker.abstracts.DMAPHandler;

public class MailboxDMAPHandler extends DMAPHandler {

    public MailboxDMAPHandler(Config users, IMailboxDataRepository repo) {
        super(users, repo);
    }
}
