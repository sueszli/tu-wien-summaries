package dslab.nameserver;

public class InvalidDomainException extends Exception {
    private static final long serialVersionUID = 1L;

    public InvalidDomainException(String message) {
        super(message);
    }

    public InvalidDomainException(String message, Throwable cause) {
        super(message, cause);
    }
}
