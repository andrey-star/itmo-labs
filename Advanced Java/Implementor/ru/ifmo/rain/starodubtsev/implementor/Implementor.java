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
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Implementor implements Impler {
	
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
		return getClass(packaj, source);
	}
	
	private String getPackage(Class<?> token) {
		String packageName = token.getPackageName();
		return packageName.isEmpty() ? "" : String.format("package %s;", packageName);
	}
	
	private String getSource(Class<?> token) throws ImplerException {
		return statement(getDeclaration(token), getBody(token));
	}
	
	private String getClass(String packaj, String source) {
		return packaj.isEmpty() ? source : String.format("%s\n\n%s", packaj, source);
	}
	
	private String statement(String declaration, String body) {
		return String.format("%s {\n%s\n}", declaration, tabbed(body, 1));
	}
	
	private String getDeclaration(Class<?> token) {
		String qualifier = token.isInterface() ? "implements" : "extends";
		return String.format("public class %s %s %s", getClassName(token), qualifier, token.getCanonicalName());
	}
	
	private String getBody(Class<?> token) throws ImplerException {
		String constructors = getConstructors(token);
		String methods = getMethods(token);
		if (constructors.isEmpty()) {
			return methods;
		}
		if (methods.isEmpty()) {
			return constructors;
		}
		return String.format("%s\n\n%s", getConstructors(token), getMethods(token));
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
		return publicConstructors.stream().map(this::getConstructor).collect(Collectors.joining("\n\n"));
		
	}
	
	private String getConstructor(Constructor<?> constructor) {
		StringBuilder declaration = new StringBuilder();
		declaration.append(String.format("public %s(%s)",
				getClassName(constructor.getDeclaringClass()),
				getParameters(constructor.getParameterTypes(), true)));
		String exceptions = getExceptions(constructor.getExceptionTypes());
		if (!exceptions.isEmpty()) {
			declaration.append(" ").append(exceptions);
		}
		String body = String.format("super(%s);", getParameters(constructor.getParameterTypes(), false));
		return statement(declaration.toString(), body);
	}
	
	private String getMethods(Class<?> token) {
		List<Method> abstractMethods = getAbstractMethods(token);
		return abstractMethods.stream().map(this::getMethod).collect(Collectors.joining("\n\n"));
	}
	
	private List<Method> getAbstractMethods(Class<?> token) {
		Set<MyMethod> methods = new HashSet<>();
		Arrays.stream(token.getMethods())
		      .filter(m -> Modifier.isAbstract(m.getModifiers()))
		      .map(MyMethod::new)
		      .collect(Collectors.toCollection(() -> methods));
		
		while (token != null) {
			Arrays.stream(token.getDeclaredMethods())
			      .filter(m -> Modifier.isAbstract(m.getModifiers()))
			      .map(MyMethod::new)
			      .collect(Collectors.toCollection(() -> methods));
			token = token.getSuperclass();
		}
		return methods.stream().map(MyMethod::getMethod).collect(Collectors.toList());
	}
	
	private String getMethod(Method method) {
		StringBuilder declaration = new StringBuilder();
		Class<?> returnType = method.getReturnType();
		declaration.append(String.format("public %s %s(%s)",
				returnType.getCanonicalName(),
				method.getName(),
				getParameters(method.getParameterTypes(), true)));
		
		String exceptions = getExceptions(method.getExceptionTypes());
		if (!exceptions.isEmpty()) {
			declaration.append(" ").append(exceptions);
		}
		
		String returnValue = getDefaultValue(returnType);
		StringBuilder body = new StringBuilder("return");
		if (!returnValue.isEmpty()) {
			body.append(" ").append(returnValue);
		}
		body.append(";");
		return statement(declaration.toString(), body.toString());
	}
	
	private String getParameters(Class<?>[] parameters, boolean typed) {
		return IntStream.range(0, parameters.length)
		                .boxed()
		                .map(i -> (typed ? parameters[i].getCanonicalName() + " " : "") + "param" + i)
		                .collect(Collectors.joining(", "));
	}
	
	private String getExceptions(Class<?>[] exceptionTypes) {
		String exceptions = Arrays.stream(exceptionTypes)
		                          .map(Class::getCanonicalName)
		                          .collect(Collectors.joining(", "));
		if (exceptions.isEmpty()) {
			return "";
		}
		return "throws " + exceptions;
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
		return body.lines().map(l -> "\t".repeat(i) + l).collect(Collectors.joining("\n"));
	}
	
	private static class MyMethod {
		
		private final Method method;
		
		public MyMethod(Method method) {
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
			if (!(o instanceof MyMethod)) {
				return false;
			}
			MyMethod other = (MyMethod) o;
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
