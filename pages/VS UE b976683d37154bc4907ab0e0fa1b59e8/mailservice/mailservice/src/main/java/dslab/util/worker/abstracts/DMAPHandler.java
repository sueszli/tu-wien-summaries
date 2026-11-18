package dslab.util.worker.abstracts;

import dslab.exception.ValidationException;
import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.model.StoredEmail;
import dslab.util.Config;
import dslab.util.worker.handlers.IDMAPHandler;

import java.util.MissingResourceException;
import java.util.stream.Collectors;

public abstract class DMAPHandler implements IDMAPHandler {
    protected final IMailboxDataRepository repo;
    private final Config users;
    protected String loggedInUser;

    public DMAPHandler(Config users, IMailboxDataRepository repo) {
        this.users = users;
        this.repo = repo;
    }

    @Override
    public String login(String username, String password) throws ValidationException {
        if (loggedInUser != null) throw new ValidationException("logout first");

        String pass;
        try {
            pass = users.getString(username);
        } catch (MissingResourceException e) {
            throw new ValidationException("unknown username");
        }

        if (!pass.equals(password)) throw new ValidationException("wrong password");

        this.loggedInUser = username;

        return "ok";
    }

    @Override
    public String list() throws ValidationException {
        validateLoggedIn();
        return repo.getAllEmailsBy(loggedInUser).stream().map(e -> String.format("%d %s %s", e.getId(), e.getFrom(), e.getSubject())).collect(Collectors.joining("\n"));
    }

    @Override
    public String show(Integer emailId) throws ValidationException {
        validateLoggedIn();

        StoredEmail email = repo.getByIdAndUser(emailId.longValue(), loggedInUser);
        if (email == null) throw new ValidationException("unknown message id");

        return String.format("from %s \n" + "to %s \n" + "subject %s \n" + "data %s", email.getFrom(), String.join(",", email.getRecipients()), email.getSubject(), email.getData());
    }

    @Override
    public String delete(Integer emailId) throws ValidationException {
        validateLoggedIn();
        repo.deleteEmailById(emailId.longValue(), loggedInUser);
        return "ok";
    }

    @Override
    public String logout() throws ValidationException {
        loggedInUser = null;
        return "ok";
    }

    private void validateLoggedIn() {
        if (loggedInUser == null) throw new ValidationException("not logged in");
    }
}
