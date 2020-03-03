package ru.ifmo.rain.starodubtsev.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.nio.file.Paths;
import java.util.*;
import java.io.File;
import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Implementor implements Impler {
	
	private static final String EXT = ".java";
	private static final String LINE_SEP = System.lineSeparator();
	private static final String TAB = "\t";
	private static final String COMMA = ",";
	private static final String SPACE = " ";
	private static final String CLASS_SUFFIX = "Impl";
	
	public static void main(String[] args) {
		if (Objects.requireNonNull(args).length != 2) {
			System.out.println(("Invalid argument(s)"));
			return;
		}
		Objects.requireNonNull(args[0]);
		Objects.requireNonNull(args[1]);
		try {
			new Implementor().implement(Class.forName(args[0]), Paths.get(args[1]));
		} catch (ClassNotFoundException e) {
			System.out.println("Invalid class: " + e.getMessage());
		} catch (InvalidPathException e) {
			System.out.println("Invalid specified path: " + e.getMessage());
		} catch (ImplerException e) {
			System.out.println(e.getMessage());
		}
	}
	
	@Override
	public void implement(Class<?> token, Path root) throws ImplerException {
		if (token == null || root == null) {
			throw new ImplerException("Invalid argument(s)");
		}
		if (unimplementable(token)) {
			throw new ImplerException("Unable to implement desired token");
		}
		
		Path filePath;
		try {
			filePath = Path.of(root.toString(), getPackageDir(token), getClassName(token) + EXT);
		} catch (InvalidPathException e) {
			throw new ImplerException("Invalid output path", e);
		}
		
		ImplementorUtils.createDirectories(filePath);
		try (BufferedWriter out = Files.newBufferedWriter(filePath)) {
			out.write(getClass(token));
		} catch (IOException e) {
			throw new ImplerException("Error when working with output file: " + filePath, e);
		}
	}
	
	String getClass(Class<?> token) throws ImplerException {
		return joinBlocks(getPackage(token), getSource(token));
	}
	
	private String getPackage(Class<?> token) {
		String packageName = token.getPackageName();
		return packageName.isEmpty() ? "" : String.format("package %s;", packageName);
	}
	
	private String getSource(Class<?> token) throws ImplerException {
		return declAndBody(getClassDeclaration(token), getBody(token));
	}
	
	private String declAndBody(String declaration, String body) {
		return String.format("%s {%s%s%s}", declaration, LINE_SEP, tabbed(body, 1), LINE_SEP);
	}
	
	private String getClassDeclaration(Class<?> token) {
		String qualifier = token.isInterface() ? "implements" : "extends";
		return String.format("public class %s %s %s", getClassName(token), qualifier, token.getCanonicalName());
	}
	
	private String getBody(Class<?> token) throws ImplerException {
		return joinBlocks(getConstructors(token), getMethods(token));
	}
	
	private String getConstructors(Class<?> token) throws ImplerException {
		if (token.isInterface()) {
			return "";
		}
		List<Constructor<?>> publicConstructors = Arrays.stream(token.getDeclaredConstructors())
		                                                .filter(c -> !Modifier.isPrivate(c.getModifiers()))
		                                                .collect(Collectors.toList());
		if (publicConstructors.isEmpty()) {
			throw new ImplerException("Cannot implement abstract class with no public constructors");
		}
		return joinBlocks(publicConstructors, this::getConstructor);
}
	
	private String getConstructor(Constructor<?> constructor) {
		Parameter[] parameters = constructor.getParameters();
		String body = String.format("super(%s);", getParameters(parameters, false));
		return getFunction(
				constructor.getModifiers(),
				parameters,
				constructor.getExceptionTypes(),
				body,
				getClassName(constructor.getDeclaringClass()));
	}
	
	private String getMethods(Class<?> token) {
		List<Method> abstractMethods = getAbstractMethods(token);
		return joinBlocks(abstractMethods, this::getMethod);
	}
	
	private List<Method> getAbstractMethods(Class<?> token) {
		Set<MethodSignature> abstractMethods = new HashSet<>();
		processMethods(token.getMethods(), abstractMethods);
		while (token != null) {
			processMethods(token.getDeclaredMethods(), abstractMethods);
			token = token.getSuperclass();
		}
		return abstractMethods.stream()
		                      .map(MethodSignature::getMethod)
		                      .collect(Collectors.toList());
	}
	
	private String getMethod(Method method) {
		Class<?> returnType = method.getReturnType();
		String returnValue = getDefaultValue(returnType);
		StringBuilder body = new StringBuilder("return");
		if (!returnValue.isEmpty()) {
			body.append(SPACE).append(returnValue);
		}
		body.append(";");
		return getFunction(
				method.getModifiers(),
				method.getParameters(),
				body.toString(),
				returnType.getCanonicalName(),
				method.getName()
		);
	}
	
	private String getFunction(int modifiers, Parameter[] parameters, String body, String... tokens) {
		return getFunction(modifiers, parameters, new Class<?>[0], body, tokens);
	}
	
	private String getFunction(int modifiers, Parameter[] parameters, Class<?>[] exceptionTypes,
	                           String body, String... tokens) {
		String declaration = getFunctionDeclaration(modifiers, parameters, exceptionTypes, tokens);
		return declAndBody(declaration, body);
	}
	
	private String getFunctionDeclaration(int modifiers, Parameter[] parameters, Class<?>[] exceptionTypes,
	                                      String... tokens) {
		int accessModifier = getAccessModifier(modifiers);
		StringBuilder declaration = new StringBuilder(
				String.join(SPACE, Modifier.toString(accessModifier), String.join(SPACE, tokens)));
		declaration.append(String.format("(%s)", getParameters(parameters, true)));
		String exceptions = getExceptions(exceptionTypes);
		if (!exceptions.isEmpty()) {
			declaration.append(" throws ").append(exceptions);
		}
		return declaration.toString();
	}
	
	private String getParameters(Parameter[] parameters, boolean typed) {
		return join(parameters,
				typed ? p -> String.join(SPACE, p.getType().getCanonicalName(), p.getName())
						: Parameter::getName, COMMA + SPACE);
	}
	
	private String getExceptions(Class<?>[] exceptionTypes) {
		return join(exceptionTypes, Class::getCanonicalName, COMMA + SPACE);
	}
	
	private void processMethods(Method[] methods, Set<MethodSignature> abstractMethods) {
		Arrays.stream(methods)
		      .filter(m -> Modifier.isAbstract(m.getModifiers()))
		      .map(MethodSignature::new)
		      .collect(Collectors.toCollection(() -> abstractMethods));
	}
	
	private String getPackageDir(Class<?> token) {
		return token.getPackageName().replace(".", File.separator);
	}
	
	private String getClassName(Class<?> token) {
		return token.getSimpleName() + CLASS_SUFFIX;
	}
	
	private String getDefaultValue(Class<?> token) {
		if (!token.isPrimitive()) {
			return "null";
		}
		if (token.equals(boolean.class)) {
			return "false";
		}
		if (token.equals(void.class)) {
			return "";
		}
		return "0";
	}
	
	private int getAccessModifier(int modifiers) {
		if (Modifier.isPrivate(modifiers)) {
			return Modifier.PRIVATE;
		}
		if (Modifier.isProtected(modifiers)) {
			return Modifier.PROTECTED;
		}
		if (Modifier.isPublic(modifiers)) {
			return Modifier.PUBLIC;
		}
		return 0;
	}
	
	private boolean unimplementable(Class<?> token) {
		int modifiers = token.getModifiers();
		return token.isPrimitive()
				|| token.isArray()
				|| Modifier.isFinal(modifiers)
				|| Modifier.isPrivate(modifiers)
				|| token == Enum.class;
	}
	
	private String joinBlocks(String... blocks) {
		return joinBlocks(blocks, Function.identity());
	}
	
	private <T> String joinBlocks(T[] blocks, Function<T, String> toString) {
		return joinBlocks(Arrays.asList(blocks), toString);
	}
	
	private <T> String joinBlocks(Collection<T> blocks, Function<T, String> toString) {
		return join(blocks, toString, LINE_SEP.repeat(2));
	}
	
	private <T> String join(T[] blocks, Function<T, String> toString, String separator) {
		return join(Arrays.asList(blocks), toString, separator);
	}
	
	private <T> String join(Collection<T> blocks, Function<T, String> toString, String separator) {
		return blocks.stream().map(toString).collect(Collectors.joining(separator));
	}
	
	private String tabbed(String body, int i) {
		var tabs = TAB.repeat(i);
		return body.lines()
		           .map(l -> tabs + l)
		           .collect(Collectors.joining(LINE_SEP));
	}
	
	private static class MethodSignature {
		
		private final Method method;
		
		public MethodSignature(Method method) {
			this.method = method;
		}
		
		public Method getMethod() {
			return method;
		}
		
		@Override
		public boolean equals(Object o) {
			if (this == o) {
				return true;
			}
			if (!(o instanceof MethodSignature)) {
				return false;
			}
			MethodSignature other = (MethodSignature) o;
			return method.getName().equals(other.method.getName())
					&& Arrays.equals(method.getParameterTypes(), other.method.getParameterTypes());
		}
		
		@Override
		public int hashCode() {
			return Objects.hash(method.getName(), Arrays.hashCode(method.getParameterTypes()));
		}
	}
}
