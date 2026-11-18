package dslab.mailbox;

import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.exception.ErrorResponseException;
import dslab.util.protocolParser.listener.DMAPListener;
import dslab.util.security.AESCryptoService;
import dslab.util.security.ICryptoService;
import dslab.util.security.RSACryptoService;
import dslab.util.sockcom.SockCom;
import dslab.util.worker.abstracts.DMAPWorker;
import dslab.util.worker.handlers.IDMAP2Handler;
import dslab.util.worker.handlers.IDMAPHandler;

import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.IOException;
import java.net.Socket;
import java.security.Key;
import java.util.Base64;

public class MailboxDMAP2Worker extends DMAPWorker implements DMAPListener, IDMAP2Handler {
    private final Socket clientSocket;
    private final Key key;
    private final ICryptoService decryptionService;
    private final String componentId;
    private SecretKey secretKey;
    private IvParameterSpec iv;

    protected MailboxDMAP2Worker(Socket clientSocket, IDMAPHandler dmapHandler, String componentId, Key key) {
        super(clientSocket, dmapHandler, "ok DMAP2.0");
        this.clientSocket = clientSocket;
        this.key = key;
        this.decryptionService = new RSACryptoService();
        this.componentId = componentId;
    }

    @Command
    public void startsecure() {
        try {
            comm = new SockCom(clientSocket);
            comm.writeLine("ok " + componentId);
            String encryptedChallenge = comm.readLine();

            if (encryptedChallenge == null) {
                errorQuit();
                return;
            }
            String encryptedResponse = processChallenge(encryptedChallenge);
            comm.writeLine(encryptedResponse);

            String confirmation = comm.readLine();
            if (!confirmation.startsWith("ok")) errorQuit();

        } catch (IOException e) {
            System.err.println("Error in socket connection");
            closeConnection();
        } catch (ErrorResponseException e) {
            System.err.println("Error message occured: " + e.getMessage());
            closeConnection();
        }
    }

    private String processChallenge(String encryptedChallenge) {
        String decryptedChallenge = decryptionService.decrypt(encryptedChallenge, key);

        String[] parts = decryptedChallenge.split("\\s+");
        if (parts.length != 4) errorQuit();
        if (!parts[0].equals("ok")) errorQuit();
        String base64Challenge = parts[1];
        String base64SecretKey = parts[2];
        String base64Iv = parts[3];

        byte[] secretKeyData = Base64.getDecoder().decode(base64SecretKey);
        secretKey = new SecretKeySpec(secretKeyData, "aes");

        byte[] ivData = Base64.getDecoder().decode(base64Iv);
        iv = new IvParameterSpec(ivData);

        AESCryptoService aesCryptoService = new AESCryptoService(secretKey, iv);
        setInputTransform(aesCryptoService::decrypt);
        setOutputTransform(aesCryptoService::encrypt);

        return aesCryptoService.encrypt("ok " + base64Challenge);
    }
}
