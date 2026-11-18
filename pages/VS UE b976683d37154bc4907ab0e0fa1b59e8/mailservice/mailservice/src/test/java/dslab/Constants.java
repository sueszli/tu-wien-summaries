package dslab;

public interface Constants {

    /**
     * Default time (in milliseconds) to wait after starting a component to test.
     */
    long COMPONENT_STARTUP_WAIT = 3000;

    /**
     * Default time (in milliseconds) to wait after shutting down a component to test.
     */
    long COMPONENT_TEARDOWN_WAIT = 3000;

    long NS_COMPONENT_STARTUP_WAIT = 300;
    long NS_COMPONENT_SHUTDOWN_WAIT = 200;
}
