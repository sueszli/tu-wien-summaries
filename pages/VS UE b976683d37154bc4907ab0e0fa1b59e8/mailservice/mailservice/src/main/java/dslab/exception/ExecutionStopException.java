package dslab.exception;

public class ExecutionStopException extends Exception {

    public ExecutionStopException(String msg, Exception reason) {
        super(msg, reason);
    }

    public ExecutionStopException(String msg) {
        super(msg);
    }

    public ExecutionStopException() {
        super();
    }
}
