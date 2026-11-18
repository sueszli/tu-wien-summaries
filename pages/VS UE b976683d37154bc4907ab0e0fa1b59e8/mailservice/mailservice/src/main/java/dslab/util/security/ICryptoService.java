package dslab.util.security;

import java.security.Key;

public interface ICryptoService {
    /**
     * Encrypts the plain text message to a Base64-String.
     *
     * @param message The string to be encrypted.
     * @param key     The key to be used in the encryption.
     * @return The Base64-encoded enrypted string.
     */
    String encrypt(String message, Key key);

    /**
     * Decrypts a Base64-encoded, encrypted string to plain text.
     *
     * @param message The Base64-encoded, encrypted message.
     * @param key     The key to be used in the encryption.
     * @return The decrypted string. Or null if message could not get decrypted.
     */
    String decrypt(String message, Key key);
}
