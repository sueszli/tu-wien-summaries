package dslab.util.protocolParser;

public class ProtocolParseException extends RuntimeException {

    public ProtocolParseException() {
        super();
    }

    public ProtocolParseException(String reason) {
        super("error " + reason);
    }

}
