package dslab.util;

import java.io.PrintStream;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

public class Logger {

    private final PrintStream out;

    public Logger(PrintStream out) {
        this.out = out;
    }

    public void println(String format, Object... args) {

        String time = LocalTime.now().truncatedTo(ChronoUnit.SECONDS).toString();
        out.printf(time + " : " + format + " \n", args);
    }
}
