package dslab;

import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.ErrorCollector;
import org.junit.rules.Timeout;

import java.util.concurrent.TimeUnit;

/**
 * Contains a generic setup for a unit test.
 */
public class TestBase {

    @Rule
    public ErrorCollector err = new ErrorCollector();

    @Rule
    public Timeout timeout = new Timeout(30, TimeUnit.SECONDS); // fail tests that do not terminate after 30 seconds

    protected TestInputStream in;
    protected TestOutputStream out;

    @Before
    public void setUpBase() throws Exception {
        in = new TestInputStream();
        out = new TestOutputStream();
    }

}
