package dslab.mailbox;

import dslab.exception.ValidationException;
import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.model.StoredEmail;
import dslab.util.Config;
import dslab.util.worker.handlers.IDMAPHandler;

public class MailboxDMAP2Handler extends MailboxDMAPHandler implements IDMAPHandler {

    public MailboxDMAP2Handler(Config users, IMailboxDataRepository repo) {
        super(users, repo);
    }

    @Override
    public String list() throws ValidationException {
        return super.list() + "\nok";
    }

    @Override
    public String show(Integer emailId) throws ValidationException {

        String baseResult = super.show(emailId);
        StoredEmail email = repo.getByIdAndUser(emailId.longValue(), loggedInUser);
        String hash = email.getHash() == null ? "" : email.getHash();
        return baseResult + "\nhash " + hash + "\nok";
    }
}
