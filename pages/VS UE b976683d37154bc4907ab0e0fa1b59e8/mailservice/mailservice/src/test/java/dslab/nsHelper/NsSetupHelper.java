package dslab.nsHelper;

import java.util.ArrayList;
import java.util.List;

public class NsSetupHelper {

    List<NsSetupModel> nsSetupModels = new ArrayList<>();

    public NsSetupHelper(String[] components) {
        for (String c : components)
            nsSetupModels.add(new NsSetupModel(c));
    }

    public void startup() throws Exception {
        try {
            for (NsSetupModel m : nsSetupModels) {
                m.start();
            }
        } catch (Exception e) {
            nsSetupModels.forEach(NsSetupModel::shutdown);
            throw e;
        }
    }

    public void shutdown() {
        nsSetupModels.forEach(NsSetupModel::shutdown);
    }

}
