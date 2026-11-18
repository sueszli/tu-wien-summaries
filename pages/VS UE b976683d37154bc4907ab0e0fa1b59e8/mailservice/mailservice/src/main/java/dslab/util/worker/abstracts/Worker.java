package dslab.util.worker.abstracts;

import at.ac.tuwien.dsg.orvell.annotation.Command;
import dslab.exception.ExecutionStopException;
import dslab.util.protocolParser.listener.IProtocolListener;

public abstract class Worker implements Runnable, IProtocolListener {

    private boolean quit = false;

    @Override
    public void run() {
        init();

        while (!quit) {
            try {
                execution();
            } catch (ExecutionStopException e) {
                System.out.println("Stop worker " + this);
                break;
            }
        }
    }


    @Override
    @Command
    public String quit() {
        quit = true;
        return "ok bye";
    }

    @Override
    public void errorQuit() {
        quit = true;
    }

    protected abstract void init();

    protected abstract void execution() throws ExecutionStopException;

}
