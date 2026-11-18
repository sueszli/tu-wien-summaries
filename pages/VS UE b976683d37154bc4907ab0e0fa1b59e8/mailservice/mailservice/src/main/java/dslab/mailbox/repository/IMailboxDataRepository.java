package dslab.mailbox.repository;

import dslab.model.Email;
import dslab.model.StoredEmail;

import java.util.List;

public interface IMailboxDataRepository {

    void addEmailToUsers(Email email);

    void deleteEmailById(Long id, String username);

    List<String> getAllUsers();

    List<StoredEmail> getAllEmailsBy(String username);

    StoredEmail getByIdAndUser(Long id, String username);

}
