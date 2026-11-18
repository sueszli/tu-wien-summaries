package dslab.mailbox.repository;

import dslab.exception.ValidationException;
import dslab.model.Email;
import dslab.model.StoredEmail;
import dslab.util.Config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class MailboxDataRepository implements IMailboxDataRepository {

    HashMap<String, ConcurrentHashMap<Long, StoredEmail>> userEmails;

    public MailboxDataRepository(Config config) {
        this.userEmails = new HashMap<>();
        config.listKeys().forEach(k -> userEmails.put(k, new ConcurrentHashMap<>()));
    }

    @Override
    public void addEmailToUsers(Email email) {
        List<String> names = email.getRecipients().stream().map(r -> r.split("@")[0]).collect(Collectors.toList());

        StoredEmail toStore = new StoredEmail(email);

        for (String n : names) {
            userEmails.get(n).put(toStore.getId(), toStore);
        }
    }

    @Override
    public void deleteEmailById(Long id, String username) throws ValidationException {
        if (userEmails.get(username).remove(id) == null) throw new ValidationException("unknown message id");
    }

    @Override
    public List<String> getAllUsers() {
        return new ArrayList<>(userEmails.keySet());
    }

    @Override
    public List<StoredEmail> getAllEmailsBy(String username) {
        ConcurrentHashMap<Long, StoredEmail> emails = userEmails.get(username);
        return new ArrayList<>(emails.values());
    }

    @Override
    public StoredEmail getByIdAndUser(Long id, String username) {
        return userEmails.get(username).get(id);
    }
}
