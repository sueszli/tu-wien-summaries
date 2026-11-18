package dslab.util.worker.handlers;

import dslab.exception.ValidationException;

public interface IDMAPHandler {

    String login(String username, String password) throws ValidationException;

    String list() throws ValidationException;

    String show(Integer emailId) throws ValidationException;

    String delete(Integer emailId) throws ValidationException;

    String logout() throws ValidationException;
}
