package dslab.util.security;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.util.Base64;

public class AESCryptoService {

    SecretKey secretKey;
    IvParameterSpec iv;

    public AESCryptoService(SecretKey secretKey, IvParameterSpec iv) {
        this.secretKey = secretKey;
        this.iv = iv;
    }

    public String encrypt(String message) {
        String result;
        try {
            Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding");
            cipher.init(Cipher.ENCRYPT_MODE, secretKey, iv);
            byte[] encryptedBytes = cipher.doFinal(message.getBytes(Charset.defaultCharset()));
            result = Base64.getEncoder().encodeToString(encryptedBytes);
        } catch (GeneralSecurityException e) {
            e.printStackTrace();
            result = null;
        }
        return result;
    }

    public String decrypt(String message) {
        String result;
        byte[] decodedBytes = Base64.getDecoder().decode(message);
        try {
            Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding");
            cipher.init(Cipher.DECRYPT_MODE, secretKey, iv);
            byte[] decryptedBytes = cipher.doFinal(decodedBytes);
            result = new String(decryptedBytes, StandardCharsets.US_ASCII);
        } catch (GeneralSecurityException e) {
            e.printStackTrace();
            result = null;
        }
        return result;
    }

}
