package ru.ifmo.rain.starodubtsev.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.util.*;
import java.io.File;
import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Implementor implements Impler {
	
	private static final String EXT = ".java";
	private static final String LINE_SEP = System.lineSeparator();
	private static final String TAB = "\t";
	private static final String COMMA = ",";
	private static final String SPACE = " ";
	private static final String CLASS_SUFFIX = "Impl";
	
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
			throw new ImplerException("Error when working with output file", e);
		}
	}
	
	String getClass(Class<?> token) throws ImplerException {
		String packaj = getPackage(token);
		String source = getSource(token);
		return joinBlocks(packaj, source);
	}
	
	private String getPackage(Class<?> token) {
		String packageName = token.getPackageName();
		return packageName.isEmpty() ? "" : String.format("package %s;", packageName);
	}
	
	private String getSource(Class<?> token) throws ImplerException {
		return declAndBody(getClassDeclaration(token), getBody(token));
	}
	
	private String joinBlocks(String... blocks) {
		return joinBlocks(Arrays.asList(blocks), Function.identity());
	}
	
	private <T> String joinBlocks(Collection<T> blocks, Function<T, String> toString) {
		return blocks.stream().map(toString).collect(Collectors.joining(LINE_SEP.repeat(2)));
	}
	
	private String declAndBody(String declaration, String body) {
		return String.format("%s {%s%s%s}", declaration, LINE_SEP, tabbed(body, 1), LINE_SEP);
	}
	
	private String getClassDeclaration(Class<?> token) {
		String qualifier = token.isInterface() ? "implements" : "extends";
		return String.format("public class %s %s %s", getClassName(token), qualifier, token.getCanonicalName());
	}
	
	private String getBody(Class<?> token) throws ImplerException {
		String constructors = getConstructors(token);
		String methods = getMethods(token);
		return joinBlocks(constructors, methods);
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
		String body = String.format("super(%s);", getParameters(constructor.getParameterTypes(), false));
		return getFunction(
				constructor.getModifiers(),
				constructor.getParameterTypes(),
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
		Set<MethodSignature> concreteMethods = new HashSet<>();
		processMethods(token::getMethods, abstractMethods, concreteMethods);
		while (token != null) {
			processMethods(token::getDeclaredMethods, abstractMethods, concreteMethods);
			token = token.getSuperclass();
		}
		return abstractMethods.stream().map(MethodSignature::getMethod).collect(Collectors.toList());
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
				method.getParameterTypes(),
				body.toString(),
				returnType.getCanonicalName(),
				method.getName()
		);
	}
	
	private String getFunction(int modifiers, Class<?>[] parameterTypes, String body, String... tokens) {
		return getFunction(modifiers, parameterTypes, new Class<?>[0], body, tokens);
	}
	
	private String getFunction(int modifiers, Class<?>[] parameterTypes, Class<?>[] exceptionTypes,
	                           String body, String... tokens) {
		String declaration = getFunctionDeclaration(modifiers, parameterTypes, exceptionTypes, tokens);
		return declAndBody(declaration, body);
	}
	
	private String getFunctionDeclaration(int modifiers, Class<?>[] parameterTypes, Class<?>[] exceptionTypes,
	                                      String... tokens) {
		int accessModifier = getAccessModifier(modifiers);
		StringBuilder declaration = new StringBuilder(String.join(SPACE, Modifier.toString(accessModifier),
				String.join(SPACE, tokens)));
		declaration.append(String.format("(%s)", getParameters(parameterTypes, true)));
		String exceptions = getExceptions(exceptionTypes);
		if (!exceptions.isEmpty()) {
			declaration.append(" throws ").append(exceptions);
		}
		return declaration.toString();
	}
	
	private String getParameters(Class<?>[] parameters, boolean typed) {
		return IntStream.range(0, parameters.length)
		                .boxed()
		                .map(i -> (typed ? parameters[i].getCanonicalName() + SPACE : "") + "param" + i)
		                .collect(Collectors.joining(COMMA + SPACE));
	}
	
	private String getExceptions(Class<?>[] exceptionTypes) {
		return Arrays.stream(exceptionTypes)
		             .map(Class::getCanonicalName)
		             .collect(Collectors.joining(COMMA + SPACE));
	}
	
	private void processMethods(Supplier<Method[]> methodSupplier, Set<MethodSignature> abstractMethods,
	                            Set<MethodSignature> concreteMethods) {
		for (Method method : methodSupplier.get()) {
			MethodSignature methodSignature = new MethodSignature(method);
			if (!Modifier.isAbstract(method.getModifiers())) {
				concreteMethods.add(methodSignature);
			} else if (!concreteMethods.contains(methodSignature)) {
				abstractMethods.add(methodSignature);
			}
		}
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
		if (token.equals(void.class)) {
			return "";
		} else if (token.equals(boolean.class)) {
			return "false";
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
		return token.isPrimitive()
				|| token.isArray()
				|| Modifier.isFinal(token.getModifiers())
				|| token == Enum.class;
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
