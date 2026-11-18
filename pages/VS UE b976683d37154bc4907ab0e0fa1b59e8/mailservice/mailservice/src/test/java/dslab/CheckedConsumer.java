package dslab;

/**
 * CheckedConsumer.
 */
@FunctionalInterface
public interface CheckedConsumer<T, E extends Exception> {
    void accept(T socket) throws E;
}
