package dslab.util.sockcom;

import dslab.exception.ErrorResponseException;
import dslab.util.protocolParser.StringTransform;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.net.SocketException;

/**
 * Encapsulates the communication through a given socket.
 */
public class SockCom implements ISockComm {

    Socket socket;
    BufferedReader in;
    PrintStream out;
    StringTransform inputTransformer = s -> s;
    StringTransform outputTransformer = s -> s;

    public SockCom(Socket socket) throws IOException {
        this.socket = socket;
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new PrintStream(socket.getOutputStream(), true);
    }

    @Override
    public String readLine() throws IOException, ErrorResponseException {
        String input = in.readLine();
        if (input == null) {
            socket.close();
            throw new SocketException("Socket closed");
        }

        String decoded = inputTransformer.transform(input);
        if (decoded.startsWith("error ")) throw new ErrorResponseException(decoded.trim());
        return decoded;
    }

    @Override
    public void writeLine(Object data) {
        out.println(outputTransformer.transform(data.toString()));
    }

    @Override
    public String writeAndReadLine(Object data) throws ErrorResponseException, IOException {
        writeLine(data);
        return readLine();
    }

    public void setInputTransformer(StringTransform inputTransformer) {
        this.inputTransformer = inputTransformer;
    }

    public void setOutputTransformer(StringTransform outputTransformer) {
        this.outputTransformer = outputTransformer;
    }

    public PrintStream out() {
        return out;
    }
}
