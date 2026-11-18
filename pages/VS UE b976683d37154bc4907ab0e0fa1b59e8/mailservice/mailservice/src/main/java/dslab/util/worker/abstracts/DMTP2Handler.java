package dslab.util.worker.abstracts;

import dslab.util.worker.handlers.IDMTP2Handler;

public abstract class DMTP2Handler extends DMTPHandler implements IDMTP2Handler {

    @Override
    public String hash(String hash) {
        validateBegin();
        getEmail().setHash(hash);
        return "ok";
    }
}
