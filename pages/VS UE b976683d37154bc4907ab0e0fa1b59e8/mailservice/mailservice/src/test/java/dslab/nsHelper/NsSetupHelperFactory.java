package dslab.nsHelper;

public class NsSetupHelperFactory {

    public static NsSetupHelper createDefaultNsHelper() {
        String[] components = {"ns-root", "ns-planet", "ns-ze", "ns-earth-planet"};
        return new NsSetupHelper(components);
    }

}
