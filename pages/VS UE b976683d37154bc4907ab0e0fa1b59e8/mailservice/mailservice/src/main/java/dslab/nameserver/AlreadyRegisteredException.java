package dslab.nameserver;

public class AlreadyRegisteredException extends Exception {
    private static final long serialVersionUID = 1L;

    public AlreadyRegisteredException(String message, Throwable cause) {
        super(message, cause);
    }

    public AlreadyRegisteredException(String message) {
        super(message);
    }
}
