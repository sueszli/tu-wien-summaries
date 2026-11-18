package dslab.util;

import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;

/**
 * Reads the configuration from a {@code .properties} file.
 */
public final class Config {

    private final ResourceBundle bundle;
    private final Map<String, Object> properties = new HashMap<>();

    /**
     * Creates an instance of Config which reads configuration data form {@code .properties} file with given name found
     * in classpath.
     *
     * @param name the name of the .properties file
     */
    public Config(String name) {
        if (name.endsWith(".properties")) {
            this.bundle = ResourceBundle.getBundle(name.substring(0, name.length() - 11));
        } else {
            this.bundle = ResourceBundle.getBundle(name);
        }
    }

    /**
     * Returns the value as String for the given key.
     *
     * @param key the property's key
     * @return String value of the property
     * @see ResourceBundle#getString(String)
     */
    public String getString(String key) {
        if (properties.containsKey(key)) {
            return properties.get(key).toString();
        }
        return this.bundle.getString(key);
    }

    /**
     * Returns the value as {@code int} for the given key.
     *
     * @param key the property's key
     * @return int value of the property
     * @throws NumberFormatException if the String cannot be parsed to an Integer
     */
    public int getInt(String key) {
        return Integer.parseInt(getString(key));
    }

    public boolean containsKey(String key) {
        return properties.containsKey(key) || bundle.containsKey(key);
    }

    /**
     * Sets the value for the given key.
     *
     * @param key   the property's key
     * @param value the value of the property
     */
    public void setProperty(String key, Object value) {
        properties.put(key, value);
    }

    /**
     * Returns all keys of this configuration.
     *
     * @return the keys
     */
    public Set<String> listKeys() {
        Set<String> keys = bundle.keySet();
        keys.addAll(properties.keySet());
        return keys;
    }
}
