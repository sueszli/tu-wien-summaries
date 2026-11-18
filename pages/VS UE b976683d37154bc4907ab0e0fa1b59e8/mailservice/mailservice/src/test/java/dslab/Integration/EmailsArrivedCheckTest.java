package dslab.Integration;

import dslab.*;
import dslab.mailbox.IMailboxServer;
import dslab.model.Email;
import dslab.monitoring.IMonitoringServer;
import dslab.nsHelper.NsSetupHelper;
import dslab.nsHelper.NsSetupHelperFactory;
import dslab.transfer.ITransferServer;
import dslab.util.Config;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.rules.Timeout;

import java.io.IOException;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertEquals;

public class EmailsArrivedCheckTest extends TestBase {

    private static final Log LOG = LogFactory.getLog(EmailsArrivedCheckTest.class);
    private static final int num_users = 100; // divisible by 2
    private static final int mails_per_connection = 5;
    private static final int mailRunner = 5;
    private static final int checkInterval = 500; // time to next check if all emails were sent
    private static final int checkRunner = 10;
    private AssertionError threadedAssertion;
    private String mailboxComponentId = "mailbox-earth-planet";
    private IMailboxServer mailbox;
    private int dmapServerPort;
    private Thread mailboxThread;
    private TestInputStream mailboxIn = new TestInputStream();

    private String transferComponentId = "transfer-1";
    private ITransferServer transfer;
    private int dmtpServerPort;
    private Thread transferThread;
    private TestInputStream transferIn = new TestInputStream();
    private TestOutputStream transferOut = new TestOutputStream();

    private String monitorComponentId = "monitoring";
    private IMonitoringServer monitor;
    private int udpServerPort;
    private Thread monitorThread;
    private TestInputStream monitorIn = new TestInputStream();

    private AtomicInteger emailId = new AtomicInteger(0);
    // Start of NS
    private NsSetupHelper nsHelper;

    public EmailsArrivedCheckTest() {
        timeout = new Timeout(12000, TimeUnit.SECONDS);
    }

    @Before
    public void setUpNs() throws Exception {
        nsHelper = NsSetupHelperFactory.createDefaultNsHelper();
        nsHelper.startup();
    }

    @After
    public void shutdownNs() throws Exception {
        nsHelper.shutdown();
    }


    @Before
    public void setUp() throws Exception {
        mailbox = ComponentFactory.createMailboxServer(mailboxComponentId, mailboxIn, out);
        transfer = ComponentFactory.createTransferServer(transferComponentId, transferIn, transferOut);
        monitor = ComponentFactory.createMonitoringServer(monitorComponentId, monitorIn, out);

        dmapServerPort = new Config(mailboxComponentId).getInt("dmap.tcp.port");
        dmtpServerPort = new Config(transferComponentId).getInt("tcp.port");
        udpServerPort = new Config(monitorComponentId).getInt("udp.port");

        transferThread = new Thread(mailbox);
        mailboxThread = new Thread(transfer);
        monitorThread = new Thread(monitor);
        monitorThread.start();
        mailboxThread.start();
        transferThread.start();

        LOG.info("Waiting for server sockets to appear");
        Sockets.waitForSocket("localhost", dmapServerPort, Constants.COMPONENT_STARTUP_WAIT);
        Sockets.waitForSocket("localhost", dmtpServerPort, Constants.COMPONENT_STARTUP_WAIT);
    }

    @After
    public void tearDown() throws Exception {

        mailboxIn.addLine("dmtpStatus");
        mailboxIn.addLine("dmapStatus");
        transferIn.addLine("connStatusTemp");
        transferIn.addLine("forwardStatusTemp");
        transferIn.addLine("shutdown"); // send "shutdown" command to command line
        mailboxIn.addLine("shutdown");
        monitorIn.addLine("shutdown"); // send "shutdown" command to command line
        Thread.sleep(Constants.COMPONENT_TEARDOWN_WAIT);
        LOG.debug(out.listen());
        if (transferThread.isAlive())
            err.addError(new Exception("Transferserver shutdown failed"));
        if (mailboxThread.isAlive())
            err.addError(new Exception("Mailboxserver shutdown failed"));
        if (monitorThread.isAlive())
            err.addError(new Exception("Monitoringserver shutdown failed"));
    }

    @Test
    public void sendEmail_shouldBeInMailbox() throws Exception {

        Email email = new Email();
        email.setSubject("Test #1");
        email.setData("Das ist ein test, kommt er an?");
        email.setRecipients(List.of("trillian@earth.planet", "test@test.asdf", "notHere@earth.planet"));
        email.setFrom("arthur@earth.planet");

        LOG.info(String.format("Send email from %s to %s", email.getFrom(), email.getRecipients()));
        sendMail(email, 1);

        Thread.sleep(2000); //wait to send

        LOG.info("Check if email arrived at trillian@earth.planet");
        checkReceivedEmail(email, "trillian", "12345", false);

        LOG.info("Check if err email arrived at arthur@earth.planet");
        Email errMail = new Email();
        errMail.setSubject("Mail Delivery Error");
        errMail.setFrom("mailer@0.0.0.0");
        checkReceivedEmail(errMail, "arthur", "23456", true);
        checkEmailAmount("arthur", "23456", 2);

    }


    @Test
    public void bigEmailPenetration() throws Exception {

        Email email = new Email();
        email.setData("Test test");
        email.setFrom("arthur@earth.planet");
        email.setRecipients(List.of());

        List<Thread> runner = new ArrayList<>();
        for (int i = 0; i < mailRunner; ++i)
            runner.add(getMailRunner(email, true));

        // test failure mail
        email.setRecipients(List.of("idontexist@unkno.wn"));
        runner.add(getMailRunner(email, false));
        email.setRecipients(List.of("idontexist@earth.planet"));
        runner.add(getMailRunner(email, false));

        runner.forEach(Thread::start);
        for (Thread thread : runner) {
            thread.join();
        }

        waitUntilForwardDone(checkInterval);

        runner.clear();
        int userPerRunner = num_users / checkRunner;
        int mailsPerUser = mailRunner * 2 * mails_per_connection;
        for (int i = 0; i < checkRunner; ++i) {
            Thread r;
            if (i + 1 == checkRunner)
                r = getCheckRunner(i * userPerRunner, num_users, mailsPerUser);
            else
                r = getCheckRunner(i * userPerRunner, (i + 1) * userPerRunner, mailsPerUser);
            runner.add(r);
        }

        runner.forEach(Thread::start);

        checkEmailAmount("arthur", "23456", 2 * mails_per_connection * num_users);

        for (Thread thread : runner) {
            thread.join();
        }

        if (threadedAssertion != null) throw threadedAssertion;
    }


    private void sendMail(Email email, int iterations) throws Exception {
        try (JunitSocketClient client = new JunitSocketClient(dmtpServerPort, err)) {
            email.setSubject("Test-" + emailId.getAndIncrement());
            LOG.info(Thread.currentThread().getName() + ": Send mail " + email);
            client.verify("ok DMTP2.0");
            for (int i = 0; i < iterations; ++i) {
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

    private void checkReceivedEmail(Email email, String user, String password, boolean isError) throws Exception {
        try (JunitSocketClient client = new JunitSocketClient(dmapServerPort, err)) {
            client.verify("ok DMAP2.0");
            client.sendAndVerify(String.format("login %s %s", user, password), "ok");

            client.send("list");
            String listResult = client.listen();
            InetAddress add = client.getSocket().getInetAddress();
            InetAddress cl = client.getSocket().getLocalAddress();
            err.checkThat(listResult, containsString(String.
                    format("%s %s", isError ? "mailer@" + "0.0.0.0" : email.getFrom(),
                            email.getSubject())));

            client.sendAndVerify("logout", "ok");
            client.sendAndVerify("quit", "ok bye");
        }
    }

    private void checkEmailAmount(String user, String password, int amount) throws IOException, InterruptedException {

        LOG.info(String.format("User %s should have %d mails", user, amount));
        try (JunitSocketClient client = new JunitSocketClient(dmapServerPort, err)) {
            client.verify("ok DMAP2.0");
            client.sendAndVerify(String.format("login %s %s", user, password), "ok");

            client.send("list");
            String listResult = client.listen();

            assertEquals(amount, listResult.split("\n").length - 1); // minus 1 because of "ok" at the end
            LOG.info(String.format("OK for %s", user));

            client.sendAndVerify("logout", "ok");
            client.sendAndVerify("quit", "ok bye");
        }
    }

    /**
     * @param fromUser   inclusive
     * @param toUser     exclusive
     * @param emailCount
     * @throws IOException
     */
    private void checkAllUsers(int fromUser, int toUser, int emailCount) throws IOException {
        LOG.info(String.format(Thread.currentThread().getName() + ": All users %d-%d should have %d mails", fromUser, toUser, emailCount));
        try (JunitSocketClient client = new JunitSocketClient(dmapServerPort, err)) {
            client.verify("ok DMAP2.0");
            for (int i = fromUser; i < toUser; ++i) {
                LOG.info(String.format(Thread.currentThread().getName() + ": Check user %s ...", i));
                client.sendAndVerify(String.format("login %s %s", i, 'p'), "ok");
                client.send("list");
                String[] listResult = client.listen().split("\n");
                assertEquals(emailCount, listResult.length - 1); // minus 1 because of "ok" at the end
                LOG.info(String.format(Thread.currentThread().getName() + ": OK for %s", i));

                client.sendAndVerify("logout", "ok");
            }
            client.sendAndVerify("quit", "ok bye");
        }
    }


    private Thread getMailRunner(Email copy, boolean addRecv) {
        return new Thread(() -> {
            Email email = new Email(copy); //create copy of mail
            for (int i = 0; i < num_users; ++i) {
                if (addRecv)
                    email.setRecipients(List.of(
                            i % num_users + "@earth.planet",
                            (i + 1) % num_users + "@earth.planet"
                    ));
                try {
                    sendMail(email, mails_per_connection);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }

    private void waitUntilForwardDone(int checkInterval) throws InterruptedException {
        while (true) {
            LOG.info("Check mails of server");
            transferIn.addLine("forwardStatus");
            Thread.sleep(checkInterval);
            List<String> servers = transferOut.getLines();
            transferOut.reset();
            String activeThreads = servers.stream()
                    .filter(s -> s.contains("ActiveThreads:"))
                    .findFirst().orElse("notfound 1");
            String queue = servers.stream()
                    .filter(s -> s.contains("Queue:"))
                    .findFirst().orElse("notfound 0");
            int activeThreadsCount = Integer.parseInt(activeThreads.split(" ")[1]);
            int queueCount = Integer.parseInt(queue.split(" ")[1]);
            LOG.info(String.format("%d threads are transfering mails and %d are in queue", activeThreadsCount, queueCount));
            if (activeThreadsCount == 0) {
                break;
            }
        }
    }

    private Thread getCheckRunner(int from, int to, int count) {
        return new Thread(() -> {
            try {
                checkAllUsers(from, to, count);
            } catch (AssertionError err) {
                threadedAssertion = err;
                throw err;
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

}
