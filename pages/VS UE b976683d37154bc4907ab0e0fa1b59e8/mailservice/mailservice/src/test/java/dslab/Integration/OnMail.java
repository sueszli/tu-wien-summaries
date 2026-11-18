package dslab.Integration;

import dslab.JunitSocketClient;
import dslab.TestBase;
import dslab.model.Email;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.List;

public class OnMail extends TestBase {

    private static final Log LOG = LogFactory.getLog(EmailsArrivedCheckTest.class);

    private static int dmtpServerPort = 11150;

    public void oneMail() throws Exception {
        Email email = new Email();

        email.setSubject("Test fail");
        email.setData("Test fail mal schauen");
        email.setRecipients(List.of("notexisting@earth.planet"));
        email.setFrom("arthur@earth.planet");
        sendMail(email, 1);

    }


    private void sendMail(Email email, int interations) throws Exception {
        try (JunitSocketClient client = new JunitSocketClient(dmtpServerPort, err)) {
            LOG.info(Thread.currentThread().getName() + ": Send mail " + email);
            client.verify("ok DMTP");
            for (int i = 0; i < interations; ++i) {
                client.sendAndVerify("begin", "ok");
                client.sendAndVerify("from " + email.getFrom(), "ok");
                client.sendAndVerify("to " + String.join(",", email.getRecipients()), "ok " + email.getRecipients().size());
                client.sendAndVerify("subject " + email.getSubject(), "ok");
                client.sendAndVerify("data " + email.getSubject(), "ok");
                client.sendAndVerify("send", "ok");
            }
            client.sendAndVerify("quit", "ok bye");
        }
    }
}
