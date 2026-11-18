package dslab.util.security;

import org.junit.Before;
import org.junit.Test;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import java.security.SecureRandom;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class AESCryptoServiceTest {

    private AESCryptoService aesCryptoService;

    @Before
    public void setup() throws Exception {
        KeyGenerator keyGenerator = KeyGenerator.getInstance("aes");
        keyGenerator.init(256);
        SecretKey secretKey = keyGenerator.generateKey();
        SecureRandom secureRandom = new SecureRandom();
        byte[] iv = new byte[16];
        secureRandom.nextBytes(iv);
        IvParameterSpec ivSpec = new IvParameterSpec(iv);
        aesCryptoService = new AESCryptoService(secretKey, ivSpec);
    }

    @Test
    public void decryptingEncryptedMessage_shouldReturnOriginalMessage() {
        String message = "Hi, this is my message";
        String enc = aesCryptoService.encrypt(message);
        String dec = aesCryptoService.decrypt(enc);
        assertThat(dec, equalTo(message));
    }
}
