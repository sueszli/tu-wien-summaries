package dslab.exception;

public class ValidationException extends RuntimeException {

    public ValidationException(String reason) {
        super(reason);
    }
}
