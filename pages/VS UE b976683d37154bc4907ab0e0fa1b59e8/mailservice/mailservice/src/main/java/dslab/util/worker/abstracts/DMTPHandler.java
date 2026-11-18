package dslab.util.worker.abstracts;

import dslab.exception.ValidationException;
import dslab.model.Email;
import dslab.util.protocolParser.ProtocolParseException;
import dslab.util.worker.handlers.IDMTPHandler;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class DMTPHandler implements IDMTPHandler {
    private final Email email = new Email();
    private boolean began = false;

    @Override
    public String begin() {
        this.began = true;
        return "ok";
    }

    @Override
    public String subject(List<String> subject) {
        validateBegin();
        if (subject.isEmpty()) throw new ValidationException("subject expected");
        email.setSubject(String.join(" ", subject));
        return "ok";
    }

    @Override
    public String data(List<String> data) {
        validateBegin();
        if (data.isEmpty()) throw new ValidationException("data expected");
        email.setData(String.join(" ", data));
        return "ok";
    }

    @Override
    public String from(String from) throws ValidationException {
        validateBegin();
        email.setFrom(from);
        return "ok";
    }

    @Override
    public String to(String to) throws ValidationException {
        validateBegin();
        List<String> recipients = Arrays.stream(to.split(",")).map(String::trim).collect(Collectors.toList());
        email.setRecipients(recipients);
        return "ok " + recipients.size();
    }


    protected void validateBegin() {
        if (!began) throw new ProtocolParseException();
    }

    protected Email getEmail() {
        return email;
    }
}
