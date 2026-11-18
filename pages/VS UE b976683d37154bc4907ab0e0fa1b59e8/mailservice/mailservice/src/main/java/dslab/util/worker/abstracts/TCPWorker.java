package dslab.util.worker.abstracts;

import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.exception.ErrorResponseException;
import dslab.exception.ExecutionStopException;
import dslab.util.protocolParser.ProtocolParser;
import dslab.util.protocolParser.StringTransform;
import dslab.util.sockcom.SockCom;

import java.io.IOException;
import java.net.Socket;

public abstract class TCPWorker extends Worker {

    private final Socket clientSocket;
    private final ProtocolParser handler;
    private final String initMessage;
    protected SockCom comm;
    private StringTransform inputTransform = (s -> s);

    protected TCPWorker(Socket clientSocket, String initMessage) {
        this.clientSocket = clientSocket;
        this.initMessage = initMessage;
        establishCommunication();
        this.handler = new ProtocolParser(this, comm.out());
    }

    @Override
    protected void init() {
        comm.writeLine(initMessage);
    }

    @Override
    protected void execution() throws ExecutionStopException {
        String input;
        try {
            input = comm.readLine();
        } catch (IOException e) {
            throw new ExecutionStopException();
        } catch (ErrorResponseException error) {
            input = error.getMessage();
        }


        if (input == null) throw new ExecutionStopException("Client connection closed."); // TODO: check this statement

        input = inputTransform.transform(input);

        handler.interpretRequest(input);
    }

    @Override
    @Command
    public String quit() {
        comm.writeLine("ok bye");
        closeConnection();
        super.quit();
        return null;
    }

    protected void closeConnection() {
        if (!clientSocket.isClosed()) try {
            clientSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void establishCommunication() {
        try {
            this.comm = new SockCom(clientSocket);
        } catch (IOException e) {
            throw new RuntimeException("Error initiating communication to client", e);
        }
    }

    public void setInputTransform(StringTransform inputTransform) {
        this.inputTransform = inputTransform;
    }

    public void setOutputTransform(StringTransform outputTransform) {
        this.handler.setOutputStringTransform(outputTransform);
    }
}
