package dslab.model;

import dslab.exception.ValidationException;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;

public class Email {

    private String subject;
    private String data;
    private String from;
    private List<String> recipients;
    private String hash;

    public Email() {
    }

    public Email(Email config) {
        subject = config.subject;
        data = config.data;
        from = config.from;
        recipients = new ArrayList<>(config.recipients);
        hash = config.hash;
    }

    public static String calculateEmailHash(Email email, SecretKeySpec sharedSecret) {
        try {
            String to = String.join(",", email.getRecipients());
            String hashInput = String.join("\n", email.from, to, email.subject, email.data);

            Mac hmac = Mac.getInstance("HmacSHA256");
            hmac.init(sharedSecret);
            byte[] hash = hmac.doFinal(hashInput.getBytes());
            return Base64.getEncoder().encodeToString(hash);

        } catch (NoSuchAlgorithmException | InvalidKeyException e) {
            return null;
        }
    }

    public static boolean invalidAddress(String email) {
        return !email.matches("^[a-zA-Z0-9_!#$%&'*+/=?`{|}~^.-]+@[a-zA-Z0-9.-]+$");
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public String getFrom() {
        return from;
    }

    public void setFrom(String from) throws ValidationException {
        if (invalidAddress(from)) throw new ValidationException("invalid email");

        this.from = from;
    }

    public List<String> getRecipients() {
        return recipients;
    }

    public void setRecipients(List<String> recipients) throws ValidationException {
        for (String rec : recipients) {
            if (invalidAddress(rec)) throw new ValidationException("inclues invalid email");
        }

        this.recipients = recipients;
    }

    public String getHash() {
        return hash;
    }

    public void setHash(String hash) {
        this.hash = hash;
    }

    public void valid() throws ValidationException {
        if (subject == null || subject.isBlank()) throw new ValidationException("no subject");
        if (data == null || data.isBlank()) throw new ValidationException("no data");
        if (from == null) throw new ValidationException("no from address");
        if (invalidAddress(from)) throw new ValidationException("invalid from address ");
        if (recipients == null || recipients.isEmpty()) throw new ValidationException("no recipients");
        for (String r : recipients)
            if (invalidAddress(r)) throw new ValidationException("includes invalid recipients address");
    }

    @Override
    public String toString() {
        return "Email{" + "subject='" + subject + '\'' + ", data='" + data + '\'' + ", from='" + from + '\'' + ", recipients=" + recipients + ", hash=" + hash + '}';
    }
}
