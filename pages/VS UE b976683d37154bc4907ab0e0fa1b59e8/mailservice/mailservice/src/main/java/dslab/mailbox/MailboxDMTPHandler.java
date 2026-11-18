package dslab.mailbox;

import dslab.exception.ValidationException;
import dslab.mailbox.repository.IMailboxDataRepository;
import dslab.model.Email;
import dslab.util.Config;
import dslab.util.worker.abstracts.DMTPHandler;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MailboxDMTPHandler extends DMTPHandler {

    Config config;
    IMailboxDataRepository dataRepository;

    public MailboxDMTPHandler(Config config, IMailboxDataRepository dataRepository) {
        this.config = config;
        this.dataRepository = dataRepository;
    }

    @Override
    public String send() throws ValidationException {
        getEmail().valid();
        dataRepository.addEmailToUsers(getEmail());

        return "ok";
    }

    @Override
    public String to(String to) throws ValidationException {
        String domain = config.getString("domain");

        List<String> emails = Stream.of(to.split(",")).filter(e -> !Email.invalidAddress(e)).filter(s -> s.split("@")[1].equals(domain)).collect(Collectors.toList());
        List<String> usernames = emails.stream().map(r -> r.trim().split("@")[0]).collect(Collectors.toList());

        List<String> storedUsers = dataRepository.getAllUsers();

        List<String> notStoredUsers = usernames.stream().filter(u -> storedUsers.stream().noneMatch(su -> su.equals(u))).collect(Collectors.toList());

        if (!notStoredUsers.isEmpty())
            throw new ValidationException("unknown recipient " + String.join(",", notStoredUsers));

        getEmail().setRecipients(emails);

        return "ok " + emails.size();
    }
}
