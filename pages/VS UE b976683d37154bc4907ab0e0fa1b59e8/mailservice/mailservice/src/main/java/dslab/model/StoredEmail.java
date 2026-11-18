package dslab.model;

import dslab.util.generator.Generator;

public class StoredEmail extends Email {

    private final Long id;

    public StoredEmail(Email config) {
        super(config);
        this.id = Generator.getID();
    }

    public Long getId() {
        return id;
    }
}
