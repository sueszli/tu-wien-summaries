package dslab.util.security;

import dslab.TestBase;
import org.junit.Before;
import org.junit.Test;

import java.security.PrivateKey;
import java.security.PublicKey;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

public class RSACryptoServiceTest extends TestBase {

    ICryptoService cryptoService;
    PrivateKey privateKey;
    PublicKey publicKey;

    @Before
    public void setup() throws Exception {
        cryptoService = new RSACryptoService();
        RSAKeyloader keyloader = new RSAKeyloader();
        keyloader.setBasePath("keys");
        keyloader.setPrivateDir("server");
        keyloader.setPublicDir("client");
        privateKey = (PrivateKey) keyloader.loadKey("mailbox-earth-planet", KeyType.PRIVATE);
        publicKey = (PublicKey) keyloader.loadKey("mailbox-earth-planet", KeyType.PUBLIC);
    }

    @Test
    public void decryptingEncryptedMessage_returnsOriginalMessage() {
        String message = "My test message";
        String enc = cryptoService.encrypt(message, publicKey);
        String dec = cryptoService.decrypt(enc, privateKey);
        assertNotNull(dec);
        assertThat(dec, equalTo(message));
    }
}
