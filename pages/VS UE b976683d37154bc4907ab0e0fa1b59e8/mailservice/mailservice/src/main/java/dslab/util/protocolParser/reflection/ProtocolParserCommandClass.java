package dslab.util.protocolParser.reflection;

import at.ac.tuwien.dsg.orvell.annotation.Command;
import at.ac.tuwien.dsg.orvell.reflection.CommandClass;
import at.ac.tuwien.dsg.orvell.reflection.CommandDeclarationException;
import at.ac.tuwien.dsg.orvell.reflection.CommandMethod;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class ProtocolParserCommandClass extends CommandClass {

    private final List<CommandMethod> commandMethods = new ArrayList<>();

    public ProtocolParserCommandClass(Class<?> clazz) throws CommandDeclarationException {
        super(clazz);
        this.initMethods(clazz);
    }

    @Override
    public List<CommandMethod> getCommandMethods() {
        return commandMethods;
    }

    private void initMethods(Class<?> clazz) {
        if (clazz == null || clazz == Objects.class) return;

        Method[] methods = clazz.getDeclaredMethods();

        for (Method m : methods) {
            if (m.isAnnotationPresent(Command.class)) {
                CommandMethod commandMethod = new CommandMethod(m);
                if (commandMethods.stream().noneMatch(c -> c.getMethod().getName().equals(m.getName())))
                    commandMethods.add(commandMethod);
            }
        }


        initMethods(clazz.getSuperclass());
//        Arrays.stream(clazz.getInterfaces()).forEach(this::initMethods);
    }
}
