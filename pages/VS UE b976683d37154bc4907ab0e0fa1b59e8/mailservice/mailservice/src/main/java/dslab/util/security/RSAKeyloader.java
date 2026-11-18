package dslab.util.security;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.Key;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

/**
 * Loads rsa keys from .der files.
 */
public class RSAKeyloader {

    KeyFactory keyFactory;
    String basePath = "keys";
    String publicDir = "client";
    String privateDir = "server";

    public RSAKeyloader() {
        try {
            this.keyFactory = KeyFactory.getInstance("RSA");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            System.err.println("This should never be thrown");
        }
    }

    /**
     * Loads a key from file.
     * <p>
     * The basepath to the keys directory as well as the specific paths to the public and private key
     * directories can be set. By default it is assumed they're located in 'keys/client' and 'keys/server'
     * where keys is the base directory and client/server is the key type specific directory.
     *
     * @param identifier The name of the .der-file the key is located in.
     * @param type       The type of key. (Either private or public).
     * @return The loaded generic key. Must normally be cast to either PrivateKey or PublicKey.
     * @throws IOException             Thrown when there is an error while trying to open the file.
     * @throws InvalidKeySpecException Thrown when the file contains invalid key specification.
     */
    public Key loadKey(String identifier, KeyType type) throws IOException, InvalidKeySpecException {
        Key key = null;
        if (type == KeyType.PRIVATE) key = loadPrivateKey(identifier);
        else if (type == KeyType.PUBLIC) key = loadPublicKey(identifier);
        return key;
    }

    private Key loadPublicKey(String identifier) throws IOException, InvalidKeySpecException {
        byte[] publicKeyData = Files.readAllBytes(Path.of(basePath, publicDir, identifier + "_pub.der"));
        X509EncodedKeySpec publicKeySpec = new X509EncodedKeySpec(publicKeyData);
        try {
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            return keyFactory.generatePublic(publicKeySpec);
        } catch (NoSuchAlgorithmException e) {
            return null;
        }
    }

    private Key loadPrivateKey(String identifier) throws IOException, InvalidKeySpecException {
        byte[] privateKeyData = Files.readAllBytes(Path.of(basePath, privateDir, identifier + ".der"));
        PKCS8EncodedKeySpec privateKeySpec = new PKCS8EncodedKeySpec(privateKeyData);
        try {
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            return keyFactory.generatePrivate(privateKeySpec);
        } catch (NoSuchAlgorithmException e) {
            return null;
        }
    }

    public void setBasePath(String basePath) {
        this.basePath = basePath;
    }

    public void setPublicDir(String publicDir) {
        this.publicDir = publicDir;
    }

    public void setPrivateDir(String privateDir) {
        this.privateDir = privateDir;
    }
}
