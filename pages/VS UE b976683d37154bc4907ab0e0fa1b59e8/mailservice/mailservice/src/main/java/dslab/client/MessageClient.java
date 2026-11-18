package dslab.client;

import at.ac.tuwien.dsg.orvell.Shell;
import at.ac.tuwien.dsg.orvell.StopShellException;
import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.ComponentFactory;
import dslab.exception.ErrorResponseException;
import dslab.exception.ValidationException;
import dslab.model.Email;
import dslab.model.InboxEmail;
import dslab.util.Config;
import dslab.util.lambdas.BinaryBoolOperator;
import dslab.util.security.*;
import dslab.util.sockcom.SockCom;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.stream.Collectors;

public class MessageClient implements IMessageClient {
    private final Shell shell;
    String componentId;
    Config config;
    InputStream in;
    PrintStream out;
    SecretKeySpec sharedSecret; //used for hashing
    private Socket mailboxSock;
    private SockCom mailSockCom;
    private boolean quit = false;
    private AESCryptoService cryptoService;

    /**
     * Creates a new client instance.
     *
     * @param componentId the id of the component that corresponds to the Config resource
     * @param config      the component config
     * @param in          the input stream to read console input from
     * @param out         the output stream to write console output to
     */
    public MessageClient(String componentId, Config config, InputStream in, PrintStream out) throws IOException {
        this.componentId = componentId;
        this.config = config;
        this.in = in;
        this.out = out;

        this.shell = new Shell(in, out);
        shell.register(this);
        shell.setPrompt(config.getString("mailbox.user") + "> ");

    }

    public static void main(String[] args) throws Exception {
        IMessageClient client = ComponentFactory.createMessageClient(args[0], System.in, System.out);
        client.run();
    }

    @Override
    public void run() {

        try {
            sharedSecret = Keys.readSecretKey(new File("keys", "hmac.key"));
        } catch (IOException e) {
            throw new RuntimeException("Could not load shared key for HMAC: " + e.getMessage(), e);
        }

        while (!quit) {
            if (mailSockCom == null || mailboxSock.isClosed()) startup();

            shell.run();

            if (!quit) out.println("Connection to mailbox lost. Reconnecting...");
        }
    }

    private void startup() {
        try {
            mailboxSock = new Socket(config.getString("mailbox.host"), config.getInt("mailbox.port"));
            mailSockCom = new SockCom(mailboxSock);
            mailSockCom.readLine();
        } catch (IOException e) {
            throw new RuntimeException("Could not establish connection to mailbox server: " + e.getMessage(), e);
        } catch (ErrorResponseException e) {
            errorQuit("error response occured: " + e.getMessage());
        }

        establishSecureConnection();
        login();
    }

    private void establishSecureConnection() {
        try {
            String res = mailSockCom.writeAndReadLine("startsecure");

            if (res == null) errorQuit("startsecure response was null");

            String[] okRes = res.split("\\s+");

            if (okRes.length != 2 && !okRes[0].equals("ok")) errorQuit("Could not establish secure connection");

            RSAKeyloader keyloader = new RSAKeyloader();
            ICryptoService rsaCryptoService = new RSACryptoService();
            PublicKey serverPub = (PublicKey) keyloader.loadKey(okRes[1], KeyType.PUBLIC);
            SecureRandom secureRandom = new SecureRandom();

            // GENERATE RESPONSE: ok <client-challenge> <secret-key> <iv>
            byte[] clientChallange = new byte[32];
            secureRandom.nextBytes(clientChallange);

            // secret-key
            KeyGenerator keyGenerator = KeyGenerator.getInstance("aes");
            keyGenerator.init(256);
            SecretKey secretKey = keyGenerator.generateKey();

            // iv
            byte[] iv = new byte[16];
            secureRandom.nextBytes(iv);
            IvParameterSpec ivSpec = new IvParameterSpec(iv);

            String b64ClientCh = Base64.getEncoder().encodeToString(clientChallange);
            String challenge = String.format("ok %s %s %s", b64ClientCh, Base64.getEncoder().encodeToString(secretKey.getEncoded()), Base64.getEncoder().encodeToString(iv));
            mailSockCom.writeLine(rsaCryptoService.encrypt(challenge, serverPub));

            cryptoService = new AESCryptoService(secretKey, ivSpec);

            // ok <client-challenge>
            String challengeCheck = mailSockCom.readLine();
            challengeCheck = cryptoService.decrypt(challengeCheck);
            if (challengeCheck == null) errorQuit("Could not decrypt client-challenge response");
            String[] checkParts = challengeCheck.split("\\s+");
            if (checkParts.length != 2 || !checkParts[0].equals("ok")) errorQuit("Challenge response was not ok");

            if (!checkParts[1].equals(b64ClientCh)) errorQuit("Challenge response includes wrong client challenge");

            // finalize
            mailSockCom.writeLine("ok");

            mailSockCom.setInputTransformer(cryptoService::decrypt);
            mailSockCom.setOutputTransformer(cryptoService::encrypt);

        } catch (IOException | InvalidKeySpecException | NoSuchAlgorithmException e) {
            errorQuit("Error while establishing secure connection: " + e.getMessage());
        } catch (ErrorResponseException e) {
            errorQuit("error response while establishing connection: " + e.getMessage());
        }
    }

    private void login() {
        String command = String.format("login %s %s", config.getString("mailbox.user"), config.getString("mailbox.password"));
        try {
            String res = mailSockCom.writeAndReadLine(command);
            if (!res.startsWith("ok")) errorQuit("Could not login because: " + res);
        } catch (IOException e) {
            errorQuit("Could not readline");
        } catch (ErrorResponseException e) {
            print(e);
        }
    }

    @Override
    @Command
    public void inbox() {

        try {
            String res = mailSockCom.writeAndReadLine("list");
            if (!res.endsWith("\nok")) {
                out.println("Inbox could not get loaded");
                return;
            }

            List<String> ids = Arrays.stream(res.split("\n")).map(e -> e.split("\\s+")[0]).filter(s -> !s.isBlank()).filter(s -> !s.startsWith("ok")).collect(Collectors.toList());

            List<InboxEmail> inbox = new ArrayList<>();
            for (String id : ids) {
                mailSockCom.writeLine("show " + id);
                InboxEmail mail = parseShowToEmail(mailSockCom.readLine());
                mail.setId(Long.valueOf(id));
                inbox.add(mail);
            }

            for (InboxEmail email : inbox) {
                print(email);
            }

            if (inbox.size() == 0) {
                out.println("No emails in inbox");
            }


        } catch (IOException e) {
            errorQuit("Mailbox connection error: " + e.getMessage());
        } catch (ErrorResponseException e) {
            print(e);
        }

    }

    private InboxEmail parseShowToEmail(String show) throws ValidationException {
        String[] lines = show.split("\n");
        if (lines.length != 6) throw new ValidationException("Show input should have 6 lines. Data was: " + show);

        InboxEmail email = new InboxEmail();

        for (String line : lines) {
            line = line.trim();
            String field = line.split(" ")[0];
            int contentIndex = field.length() + 1;

            if (contentIndex > line.length()) continue;

            switch (field) {
                case "from":
                    email.setFrom(line.substring(contentIndex));
                    break;
                case "to":
                    email.setRecipients(Arrays.stream(line.substring(contentIndex).split(",")).collect(Collectors.toList()));
                    break;
                case "subject":
                    email.setSubject(line.substring(contentIndex));
                    break;
                case "data":
                    email.setData(line.substring(contentIndex));
                    break;
                case "hash":
                    email.setHash(line.substring(contentIndex));
                    break;
                case "ok":
                    break;
                default:
                    throw new ValidationException("Could not parse field " + field);
            }
        }

        email.valid();
        return email;
    }

    private void print(InboxEmail email) {
        String res = "\n" + "ID: " + email.getId() + "\n" + "From: " + email.getFrom() + "\n" + "To: " + String.join(", ", email.getRecipients()) + "\n" + "Subject: " + email.getSubject() + "\n\n" + email.getData() + "\n\n" + "================";
        out.println(res);
    }

    private void print(ErrorResponseException reason) {
        out.println("error response occured: " + reason.getMessage());
    }

    @Override
    @Command
    public void delete(String id) throws IOException {
        try {
            String res = mailSockCom.writeAndReadLine("delete " + id);
            checkRes("ok", res, String::equals, "Protocol error while deleting message " + id);
        } catch (ErrorResponseException e) {
            print(e);
        }
    }

    @Override
    @Command
    public void verify(String id) throws IOException {
        try {
            mailSockCom.writeLine("show " + id);
            Email email = parseShowToEmail(mailSockCom.readLine());
            String actualHash = Email.calculateEmailHash(email, sharedSecret);

            if (actualHash == null || !actualHash.equals(email.getHash()))
                out.println("error email integrity not given");
            else out.println("ok");

        } catch (ErrorResponseException e) {
            print(e);
        }
    }

    @Override
    @Command
    public void msg(String to, String subject, String data) throws IOException {

        Email email = new Email();
        email.setFrom(config.getString("transfer.email"));
        email.setRecipients(Arrays.stream(to.split(",")).map(String::trim).collect(Collectors.toList()));
        email.setSubject(subject);
        email.setData(data);

        email.valid();
        String hashB64 = Email.calculateEmailHash(email, sharedSecret);
        email.setHash(hashB64);

        Socket socket = new Socket(config.getString("transfer.host"), config.getInt("transfer.port"));
        SockCom com = new SockCom(socket);

        try {
            com.readLine();
            com.writeLine("begin");
            checkRes("ok", com.readLine(), String::equals, "Could not 'begin' protocol");
            com.writeLine("from " + email.getFrom());
            checkRes("ok", com.readLine(), String::equals, "ProtocolError using '" + email.getFrom() + "' as 'from'");
            com.writeLine("to " + String.join(",", email.getRecipients()));
            checkRes("ok", com.readLine(), String::startsWith, "ProtocolError using '" + String.join(",", email.getRecipients()) + "' as 'to'");
            com.writeLine("subject " + email.getSubject());
            checkRes("ok", com.readLine(), String::equals, "ProtocolError using '" + email.getSubject() + "' as 'subject'");
            com.writeLine("data " + email.getData());
            checkRes("ok", com.readLine(), String::equals, "ProtocolError using '" + email.getData() + "' as 'data'");
            com.writeLine("hash " + email.getHash());
            checkRes("ok", com.readLine(), String::equals, "ProtocolError using '" + email.getHash() + "' as 'hash'");
            com.writeLine("send");
            checkRes("ok", com.readLine(), String::equals, "ProtocolError sending mail " + email);
        } catch (ErrorResponseException e) {
            print(e);
        }

        try {
            com.writeLine("quit");
            socket.close();
        } catch (IOException ignored) {
        }
    }

    private void checkRes(String expected, String actual, BinaryBoolOperator<String> op, String errorMsg) throws ValidationException {
        if (!op.op(actual, expected))
            throw new ValidationException(errorMsg + ": got '" + actual + "' but expected '" + expected + "'");
    }

    @Override
    @Command
    public void shutdown() {
        quit = true;
        try {
            mailboxSock.close();
        } catch (IOException ignore) {
        }

        throw new StopShellException();
    }

    private void errorQuit(String reason) {
        try {
            if (!mailboxSock.isClosed()) mailboxSock.close();
        } catch (IOException ignored) {
        }

        throw new RuntimeException(reason);
    }

}
