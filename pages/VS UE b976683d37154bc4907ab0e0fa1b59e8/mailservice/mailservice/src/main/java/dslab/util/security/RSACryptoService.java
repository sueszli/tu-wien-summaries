package dslab.util.security;

import javax.crypto.Cipher;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.util.Base64;

public class RSACryptoService implements ICryptoService {

    @Override
    public String encrypt(String message, Key key) {
        String result;
        try {
            Cipher cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");
            cipher.init(Cipher.ENCRYPT_MODE, key);
            byte[] encMessage = message.getBytes(Charset.defaultCharset());
            result = encode(cipher.doFinal(encMessage));
        } catch (GeneralSecurityException e) {
            result = null;
        }
        return result;
    }

    @Override
    public String decrypt(String message, Key key) {
        String result;
        byte[] decodedBytes = decode(message);
        try {
            Cipher cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");
            cipher.init(Cipher.DECRYPT_MODE, key);
            byte[] decryptedBytes = cipher.doFinal(decodedBytes);
            result = new String(decryptedBytes, StandardCharsets.US_ASCII);
        } catch (GeneralSecurityException e) {
            result = null;
        }
        return result;
    }

    private byte[] decode(String message) {
        return Base64.getDecoder().decode(message);
    }

    private String encode(byte[] byteData) {
        return Base64.getEncoder().encodeToString(byteData);
    }
}
