package dslab.util.worker.handlers;

import dslab.exception.ValidationException;

import java.util.List;

public interface IDMTPHandler {
    String begin();

    String subject(List<String> subject);

    String data(List<String> data);

    String from(String from) throws ValidationException;

    String to(String to) throws ValidationException;

    String send() throws ValidationException;
}
