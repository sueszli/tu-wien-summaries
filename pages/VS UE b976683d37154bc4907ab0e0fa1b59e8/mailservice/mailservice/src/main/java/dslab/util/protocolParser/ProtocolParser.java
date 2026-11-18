package dslab.util.protocolParser;

import at.ac.tuwien.dsg.orvell.Command;
import at.ac.tuwien.dsg.orvell.CommandUsageException;
import at.ac.tuwien.dsg.orvell.Context;
import at.ac.tuwien.dsg.orvell.Input;
import at.ac.tuwien.dsg.orvell.io.InputParser;
import at.ac.tuwien.dsg.orvell.reflection.CommandClass;
import at.ac.tuwien.dsg.orvell.reflection.CommandMethod;
import dslab.exception.ValidationException;
import dslab.util.protocolParser.listener.IProtocolListener;
import dslab.util.protocolParser.reflection.ProtocolParaserMethodAdapter;
import dslab.util.protocolParser.reflection.ProtocolParserCommandClass;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ProtocolParser implements IProtocolParser {

    private final InputParser parser = new InputParser();
    private final Map<String, Command> commands;
    private final Context ctx;
    private final IProtocolListener registerObj;
    private StringTransform outputStringTransform = (s -> s);

    public ProtocolParser(IProtocolListener registerObj, PrintStream printStream) {
        this.registerObj = registerObj;
        commands = new HashMap<>();
        ctx = new Context(printStream, System.err, commands);

        register(registerObj);
    }

    private void register(IProtocolListener registObj) {
        register(registObj.getClass(), registObj);
    }

    public void interpretRequest(String req) {

        Input input;
        try {
            input = parser.parse(req);
        } catch (Exception e) {
            printErr("protocol or server error");
            registerObj.errorQuit();
            return;
        }

        Command command = getCommand(input);

        if (command == null) {
            printErr("protocol error");
            registerObj.errorQuit();
            return;
        }

        try {
            command.execute(input, ctx);
        } catch (ValidationException ex) {
            printErr(ex.getMessage());
        } catch (CommandUsageException | ProtocolParseException e) {
            printErr("protocol error");
            registerObj.errorQuit();
        }
    }

    private void register(Class<? extends IProtocolListener> clazz, IProtocolListener registerObj) {
        CommandClass commandClass = new ProtocolParserCommandClass(clazz);
        List<CommandMethod> commandMethods = commandClass.getCommandMethods();

        Map<String, Command> commands = new HashMap<>(commandMethods.size());

        for (CommandMethod m : commandMethods) {
            ProtocolParaserMethodAdapter command = new ProtocolParaserMethodAdapter(m, registerObj);

            commands.put(m.getName(), command);
        }

        this.commands.putAll(commands);
    }

    private void printErr(String reason) {
        String transformed = outputStringTransform.transform("error " + reason);
        this.ctx.out().println(transformed);
    }

    private Command getCommand(Input input) {
        return this.commands.get(input.getCommand());
    }

    public void setOutputStringTransform(StringTransform outputStringTransform) {
        this.outputStringTransform = outputStringTransform;
        for (Command command : commands.values()) {
            ProtocolParaserMethodAdapter a = (ProtocolParaserMethodAdapter) command;
            a.setOutputTransform(outputStringTransform);
        }
    }
}
