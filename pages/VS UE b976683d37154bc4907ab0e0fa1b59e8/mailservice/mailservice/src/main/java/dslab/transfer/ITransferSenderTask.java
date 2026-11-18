package dslab.transfer;

import dslab.exception.NoOkResponseException;

import java.io.IOException;

public interface ITransferSenderTask extends Runnable {
    void sendEmail() throws IOException, NoOkResponseException;
}
