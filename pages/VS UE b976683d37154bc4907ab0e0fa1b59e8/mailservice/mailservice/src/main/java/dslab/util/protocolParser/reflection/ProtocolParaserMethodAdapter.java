package dslab.util.protocolParser.reflection;


import at.ac.tuwien.dsg.orvell.*;
import at.ac.tuwien.dsg.orvell.reflection.CommandMethod;
import dslab.exception.ValidationException;
import dslab.util.protocolParser.ProtocolParseException;
import dslab.util.protocolParser.StringTransform;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Parameter;
import java.util.List;

public class ProtocolParaserMethodAdapter implements Command {
    private final CommandMethod method;
    private final Object object;
    private StringTransform outputTransform = (s -> s);

    public ProtocolParaserMethodAdapter(CommandMethod method, Object object) {
        this.method = method;
        this.object = object;
    }

    @Override
    public void execute(Input input, Context context) {
        this.validate(input);

        try {
            Object[] args = this.prepareArguments(input);
            if (this.method.hasContextParameter()) {
                args[0] = context;
            }

            Object result = this.method.getMethod().invoke(this.object, args);
            if (result != null) {
                String stringResult = outputTransform.transform(String.valueOf(result));
                context.out().println(stringResult);
            }

        } catch (IllegalAccessException var5) {
            throw new RuntimeException(var5);
        } catch (InvocationTargetException var6) {
            if (var6.getCause() instanceof StopShellException) {
                throw (StopShellException) var6.getCause();
            } else if (var6.getCause() instanceof ExitException) {
                throw (ExitException) var6.getCause();
            } else if (var6.getCause() instanceof ValidationException) {
                throw (ValidationException) var6.getCause();
            } else if (var6.getCause() instanceof ProtocolParseException) {
                throw (ProtocolParseException) var6.getCause();
            } else {
                throw new CommandExecutionException(var6.getCause());
            }
        }
    }

    private Object[] prepareArguments(Input input) {
        Parameter[] parameters = this.method.getParameters();
        Object[] values = new Object[parameters.length];
        int offset = this.method.hasContextParameter() ? 1 : 0;


        if (parameters.length == 1 && List.class.isAssignableFrom(parameters[0].getType())) {
            return new List[]{input.getArguments()};
        }

        for (int i = 0; i < input.argc(); ++i) {
            Parameter parameter = parameters[i + offset];
            Class<?> type = parameter.getType();
            String inputValue = input.getArguments().get(i);
            Object value = inputValue;
            if (Integer.TYPE.isAssignableFrom(type) || Integer.class.isAssignableFrom(type)) {
                try {
                    value = Integer.parseInt(inputValue);
                } catch (NumberFormatException var11) {
                    throw new CommandExecutionException(var11);
                }
            }

            values[i + offset] = value;
        }

        return values;
    }

    private void validate(Input input) {

        Parameter[] parameters = this.method.getParameters();

        if (parameters.length == 1 && List.class.isAssignableFrom(parameters[0].getType())) return;

        int expected = parameters.length - (this.method.hasContextParameter() ? 1 : 0);
        if (expected != input.argc()) {
            throw new CommandUsageException("Expected " + expected + " arguments");
        }
    }

    public void setOutputTransform(StringTransform outputTransform) {
        this.outputTransform = outputTransform;
    }


}
