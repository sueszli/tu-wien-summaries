package dslab.util.security;

import javax.crypto.spec.SecretKeySpec;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

/**
 * Reads encryption keys from the file system.
 */
public final class Keys {

    private Keys() {
        // util class
    }

    /**
     * Reads an RSA private key from the given file.
     * The key files must be in DER format so JAVA can read it.
     *
     * @param file the file containing the RSA key
     * @return a PrivateKey instance
     * @throws IOException           if an exception occurred while reading the key file
     * @throws NullPointerException  if encoded key is null
     * @throws IllegalStateException if an error occurred in the java security api
     */
    public static PrivateKey readPrivateKey(File file) throws IOException, NullPointerException, IllegalStateException {
        try {
            byte[] fileContent = Files.readAllBytes(file.toPath());
            PKCS8EncodedKeySpec spec = new PKCS8EncodedKeySpec(fileContent);

            return createPrivateKey(spec);
        } catch (IOException e) {
            throw new IOException(String.format("Cannot read key file %s.", file.getCanonicalPath()));
        } catch (NullPointerException e) {
            throw new NullPointerException(String.format("Key seems to be empty from file %s", file.getCanonicalPath()));
        }
    }

    /**
     * Reads an RSA public key from the given file.
     * The key files must be in DER format so JAVA can read it.
     *
     * @param file the file containing the RSA key
     * @return a PublicKey instance
     * @throws IOException           if an exception occurred while reading the key file
     * @throws NullPointerException  if encoded key is null
     * @throws IllegalStateException if an error occurred in the java security api
     */
    public static PublicKey readPublicKey(File file) throws IOException, NullPointerException, IllegalStateException {
        try {
            byte[] fileContent = Files.readAllBytes(file.toPath());
            X509EncodedKeySpec spec = new X509EncodedKeySpec(fileContent);

            return createPublicKey(spec);
        } catch (IOException e) {
            throw new IOException(String.format("Cannot read key file %s.", file.getCanonicalPath()), e);
        } catch (NullPointerException e) {
            throw new NullPointerException(String.format("Key seems to be empty from file %s", file.getCanonicalPath()));
        }
    }

    /**
     * Reads the {@link SecretKeySpec} from the given location which is expected to contain a HMAC SHA-256 key.
     *
     * @param file the path to key located in the file system
     * @return the secret key
     * @throws IOException if an I/O error occurs or the security provider cannot handle the file
     */
    public static SecretKeySpec readSecretKey(File file) throws IOException {
        try (FileInputStream in = new FileInputStream(file)) {
            byte[] keyBytes = new byte[1024];
            if (in.read(keyBytes) < 0) {
                throw new IOException(String.format("Cannot read key file %s.", file.getCanonicalPath()));
            }

            byte[] input = hexStringToByteArray(new String(keyBytes));
            return new SecretKeySpec(input, "HmacSHA256");
        }
    }

    private static PrivateKey createPrivateKey(KeySpec spec) {
        try {
            return getRsaKeyFactory().generatePrivate(spec);
        } catch (InvalidKeySpecException e) {
            throw new IllegalStateException("Error creating private key", e);
        }
    }

    private static PublicKey createPublicKey(KeySpec spec) {
        try {
            return getRsaKeyFactory().generatePublic(spec);
        } catch (InvalidKeySpecException e) {
            throw new IllegalStateException("Error creating private key", e);
        }
    }

    private static KeyFactory getRsaKeyFactory() throws IllegalStateException {
        try {
            return KeyFactory.getInstance("RSA");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("Error creating RSA key factory with default security provider", e);
        }
    }

    public static byte[] hexStringToByteArray(String s) {
        int len = s.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16));
        }
        return data;
    }

}
