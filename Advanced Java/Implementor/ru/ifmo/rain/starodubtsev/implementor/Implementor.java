package ru.ifmo.rain.starodubtsev.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Implementor implements Impler {
	
	public static void main(String[] args) throws ImplerException {
		System.out.println("________________________________________________________");
		System.out.println(new Implementor().generateClass(TestAbstract2.class));
	}
	
	@Override
	public void implement(Class<?> token, Path root) throws ImplerException {
		if (token == null || root == null) {
			throw new ImplerException("Invalid argument(s)");
		}
		if (unimplementable(token)) {
			throw new ImplerException("Unable to implement");
		}
		
		Path filePath;
		try {
			filePath = Path.of(root.toString(), getPackageDir(token), getClassName(token) + ".java");
		} catch (InvalidPathException e) {
			throw new ImplerException(e);
		}
		
		ImplementorUtils.createDirectories(filePath);
		try (BufferedWriter out = Files.newBufferedWriter(filePath)) {
			out.write(generateClass(token));
		} catch (IOException e) {
			throw new ImplerException(e);
		}
	}
	
	private String generateClass(Class<?> token) throws ImplerException {
		String packaj = getPackage(token);
		String source = getSource(token);
		return joinBlocks(packaj, source);
	}
	
	private String getPackage(Class<?> token) {
		String packageName = token.getPackageName();
		return packageName.isEmpty() ? "" : String.format("package %s;", packageName);
	}
	
	private String getSource(Class<?> token) throws ImplerException {
		return statement(getClassDeclaration(token), getBody(token));
	}
	
	private String joinBlocks(String... blocks) {
		return joinBlocks(Arrays.asList(blocks), Function.identity());
	}
	
	private <T> String joinBlocks(Collection<T> blocks, Function<T, String> toString) {
		return blocks.stream().map(toString).collect(Collectors.joining("\n\n"));
	}
	
	private String statement(String declaration, String body) {
		return String.format("%s {\n%s\n}", declaration, tabbed(body, 1));
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
		Set<MethodSignature> methods = new HashSet<>();
		addMethods(token::getMethods, methods);
		while (token != null) {
			addMethods(token::getDeclaredMethods, methods);
			token = token.getSuperclass();
		}
		return methods.stream().map(MethodSignature::getMethod).collect(Collectors.toList());
	}
	
	private String getMethod(Method method) {
		Class<?> returnType = method.getReturnType();
		String returnValue = getDefaultValue(returnType);
		StringBuilder body = new StringBuilder("return");
		if (!returnValue.isEmpty()) {
			body.append(" ").append(returnValue);
		}
		body.append(";");
		return getFunction(
				method.getParameterTypes(),
				method.getExceptionTypes(),
				body.toString(),
				returnType.getCanonicalName(),
				method.getName()
		);
	}
	
	private String getFunction(Class<?>[] parameterTypes,
	                           Class<?>[] exceptionTypes,
	                           String body,
	                           String... args) {
		String declaration = getFunctionDeclaration(parameterTypes, exceptionTypes, args);
		return statement(declaration, body);
	}
	
	private String getFunctionDeclaration(Class<?>[] parameterTypes, Class<?>[] exceptionTypes, String... args) {
		StringBuilder declaration = new StringBuilder(String.join(
				" ",
				"public",
				String.join(" ", args)));
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
		                .map(i -> (typed ? parameters[i].getCanonicalName() + " " : "") + "param" + i)
		                .collect(Collectors.joining(", "));
	}
	
	private String getExceptions(Class<?>[] exceptionTypes) {
		return Arrays.stream(exceptionTypes)
		             .map(Class::getCanonicalName)
		             .collect(Collectors.joining(", "));
	}
	
	private void addMethods(Supplier<Method[]> methodSupplier, Set<MethodSignature> methods) {
		Arrays.stream(methodSupplier.get())
		      .filter(m -> Modifier.isAbstract(m.getModifiers()))
		      .map(MethodSignature::new)
		      .collect(Collectors.toCollection(() -> methods));
	}
	
	private String getPackageDir(Class<?> token) {
		return token.getPackageName().replace(".", File.separator);
	}
	
	private String getClassName(Class<?> token) {
		return token.getSimpleName() + "Impl";
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
	
	private boolean unimplementable(Class<?> token) {
		return token.isPrimitive()
				|| token.isArray()
				|| Modifier.isFinal(token.getModifiers())
				|| token == Enum.class;
	}
	
	private String tabbed(String body, int i) {
		String tabs = "\t".repeat(i);
		return body.lines()
		           .map(l -> tabs + l)
		           .collect(Collectors.joining("\n"));
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
					&& method.getReturnType().equals(other.method.getReturnType())
					&& Arrays.equals(method.getParameterTypes(), other.method.getParameterTypes());
		}
		
		@Override
		public int hashCode() {
			return Objects.hash(Arrays.hashCode(method.getParameterTypes()), method.getName(), method.getReturnType());
		}
	}
}
