package dslab.util.sockcom;

import dslab.exception.ErrorResponseException;

import java.io.IOException;

public interface ISockComm {

    String readLine() throws IOException, ErrorResponseException;

    void writeLine(Object data);

    String writeAndReadLine(Object data) throws ErrorResponseException, IOException;
}
