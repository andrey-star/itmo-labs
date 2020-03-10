package ru.ifmo.rain.starodubtsev.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.JarImpler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.util.*;
import java.util.function.Function;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.net.URL;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.security.CodeSource;

/**
 * Code generating implementation of the {@code Impler} and {@code JarImpler} interfaces.
 * Capable of generating {@code .java} and {@code .jar} files for classes,
 * implementing the provided {@code Class} token using {@code Reflection API}.
 *
 * @author Andrey Starodubtsev
 * @see Impler
 * @see JarImpler
 * @see java.lang.reflect
 * @see ImplementorUtils
 */
public class Implementor implements Impler, JarImpler {
	
	/**
	 * Extension for generated {@code .java} files.
	 */
	private static final String JAVA = ".java";
	/**
	 * Extension for generated {@code .class} files.
	 */
	private static final String CLASS = ".class";
	/**
	 * System defined line separator for generated {@code .java} files.
	 */
	private static final String LINE_SEP = System.lineSeparator();
	/**
	 * Tab code indentation for generated {@code .java} files.
	 */
	private static final String TAB = "\t";
	/**
	 * Space code indentation for generated {@code .java} files.
	 */
	private static final String SPACE = " ";
	/**
	 * Comma separator for generated {@code .java} files.
	 */
	private static final String COMMA = ",";
	/**
	 * Suffix, defining the name of the resulting class.
	 */
	private static final String CLASS_SUFFIX = "Impl";
	/**
	 * Command line option. When present, runs the application in {@code jar} mode.
	 */
	private static final String JAR_MODE = "--jar";
	
	/**
	 * Default constructor. Creates a new instance of {@code Implementor}.
	 */
	public Implementor() {
	}
	
	/**
	 * Main method. A command line utility for {@code Implementor}.
	 * Supports two modes
	 * <ol>
	 *     <li><b>java</b>: {@code <className> <outputPath>}.
	 *     Creates a {@code .java} file by passing the arguments to {@link #implement(Class, Path)}.</li>
	 *     <li><b>jar</b>: {@code -jar <className> <outputPath>}.
	 *     Creates a {@code .jar} file by passing the arguments to {@link #implementJar(Class, Path)}.</li>
	 * </ol>
	 * If any arguments are invalid or an error occurs, execution is stopped
	 * and a message describing the issue is displayed.
	 *
	 * @param args list of command line arguments
	 */
	public static void main(String[] args) {
		Objects.requireNonNull(args);
		if (args.length < 2 || args.length > 3
				|| (args.length == 3 && !JAR_MODE.equals(args[0]))) {
			System.out.println(("Invalid argument(s)"));
			return;
		}
		Objects.requireNonNull(args[0]);
		Objects.requireNonNull(args[1]);
		try {
			if (args.length == 2) {
				new Implementor().implement(Class.forName(args[0]), Paths.get(args[1]));
			} else {
				new Implementor().implementJar(Class.forName(args[1]), Paths.get(args[2]));
			}
		} catch (ClassNotFoundException e) {
			System.out.println("Invalid class: " + e.getMessage());
		} catch (InvalidPathException e) {
			System.out.println("Invalid specified path: " + e.getMessage());
		} catch (ImplerException e) {
			System.out.println(e.getMessage());
		}
	}
	
	/**
	 * Produces code implementing the class or interface specified by provided {@code token}.
	 * The generated {@code .java} file location is specified by {@code root}.
	 */
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
			filePath = getFullPath(root, token);
		} catch (InvalidPathException e) {
			throw new ImplerException("Invalid output path", e);
		}
		
		ImplementorUtils.createDirectories(filePath);
		try (BufferedWriter out = Files.newBufferedWriter(filePath)) {
			out.write(encode(getClass(token)));
		} catch (IOException e) {
			throw new ImplerException("Error when working with output file: " + filePath, e);
		}
	}
	
	/**
	 * Produces {@code .jar} file implementing class or interface specified by provided {@code token}.
	 * The generated {@code .jar} file location is specified by {@code jarFile}.
	 */
	@Override
	public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
		if (token == null || jarFile == null) {
			throw new ImplerException("Invalid argument(s)");
		}
		ImplementorUtils.createDirectories(jarFile);
		Path temp = ImplementorUtils.createTempDirectory(jarFile.toAbsolutePath().getParent());
		try {
			implement(token, temp);
			compile(token, temp);
			buildJar(token, jarFile, temp);
		} finally {
			ImplementorUtils.cleanDirectory(temp);
		}
	}
	
	/**
	 * Compiles the {@code token} implementation {@code .java} file.
	 * Stores the resulting {@code .class} file at {@code temp}.
	 *
	 * @param token type token, the implementation of which is stored at {@code temp}
	 * @param temp  working directory containing the source of {@code token} implementation
	 * @throws ImplerException if an error occurs during compilation
	 * @see JavaCompiler
	 */
	private void compile(Class<?> token, Path temp) throws ImplerException {
		Path tokenClassPath;
		try {
			CodeSource codeSource = token.getProtectionDomain().getCodeSource();
			if (codeSource == null) {
				throw new ImplerException("Invalid token CodeSource");
			}
			URL url = codeSource.getLocation();
			if (url == null) {
				throw new ImplerException("Invalid token CodeSource location");
			}
			String path = url.getPath();
			if (path.isEmpty()) {
				throw new ImplerException("Invalid token CodeSource location path");
			}
			if (path.startsWith("/")) {
				path = path.substring(1);
			}
			tokenClassPath = Path.of(path);
		} catch (InvalidPathException | ImplerException e) {
			throw new ImplerException("Cannot find token classpath", e);
		}
		
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		String[] args = {"-cp",
				temp.toString() + File.pathSeparator + tokenClassPath.toString(),
				getFullPath(temp, token).toString()};
		System.out.println(getFullPath(temp, token).toString());
		if (compiler == null || compiler.run(null, null, null, args) != 0) {
			throw new ImplerException("Failed to compile class");
		}
	}
	
	/**
	 * Builds a {@code .jar} file containing compiled implementation of {@code token}.
	 *
	 * @param token   type token, the implementation of which is stored at {@code temp}
	 * @param jarFile resulting {@code .jar} file destination
	 * @param temp    directory containing the compiled {@code .class} files
	 * @throws ImplerException if en error occurs when working with {@code .jar} file
	 */
	private void buildJar(Class<?> token, Path jarFile, Path temp) throws ImplerException {
		Manifest manifest = new Manifest();
		Attributes attributes = manifest.getMainAttributes();
		attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");
		try (JarOutputStream out = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
			String localName = getPackageDir(token, "/") + "/" + getClassName(token) + CLASS;
			System.out.println(localName);
			out.putNextEntry(new ZipEntry(localName));
			Files.copy(temp.resolve(localName), out);
		} catch (IOException e) {
			throw new ImplerException("Error when working with jar file", e);
		}
	}
	
	/**
	 * Generates full {@code token} implementation class source code.
	 * Generates a compile error free implementation of {@code token},
	 * ready to be exported to a {@code .java} file.
	 *
	 * @param token the type token to be implemented
	 * @return a {@code String} representation of {@code token} implementation
	 * @throws ImplerException if the implementation cannot be generated,
	 *                         due to absence of non-private constructors of {@code token}
	 * @see #getPackage(Class)
	 * @see #getSource(Class)
	 */
	private String getClass(Class<?> token) throws ImplerException {
		return joinBlocks(getPackage(token), getSource(token));
	}
	
	/**
	 * Returns the package declaration for specified {@code token}.
	 *
	 * @param token the type token
	 * @return a {@code String} representing the package declaration of provided {@code token},
	 * or an empty {@code String} if the package is default.
	 */
	private String getPackage(Class<?> token) {
		String packageName = token.getPackageName();
		return packageName.isEmpty() ? "" : String.format("package %s;", packageName);
	}
	
	/**
	 * Generates {@code token} implementation class content.
	 * Returns a {@code String} representing the contents of {@code token} implementation,
	 * containing class declaration and body.
	 *
	 * @param token the type token to be implemented
	 * @return a {@code String} representation of {@code token} implementation contents
	 * @throws ImplerException if the content cannot be generated,
	 *                         due to absence of non-private constructors of {@code token}
	 * @see #getClassDeclaration(Class)
	 * @see #getBody(Class)
	 */
	private String getSource(Class<?> token) throws ImplerException {
		return declAndBody(getClassDeclaration(token), getBody(token));
	}
	
	/**
	 * Returns the class declaration of {@code token} implementation.
	 * Determines whether the {@code token} is an {@code interface} or a {@code class},
	 * and generates a correct declaration of the class, implementing {@code token}.
	 *
	 * @param token the type token
	 * @return a {@code String} representation of {@code token} implementation class declaration
	 * @see #getClassName(Class)
	 */
	private String getClassDeclaration(Class<?> token) {
		String qualifier = token.isInterface() ? "implements" : "extends";
		return String.format("public class %s %s %s", getClassName(token), qualifier, token.getCanonicalName());
	}
	
	/**
	 * Returns the class body of {@code token} implementation.
	 *
	 * @param token the type token
	 * @return a {@code String} representation of {@code token} implementation class body
	 * @throws ImplerException if the body cannot be generated,
	 *                         due to absence of non-private constructors of {@code token}
	 * @see #getConstructor(Class)
	 * @see #getMethods(Class)
	 * @see #joinBlocks(String...)
	 */
	private String getBody(Class<?> token) throws ImplerException {
		return joinBlocks(getConstructor(token), getMethods(token));
	}
	
	/**
	 * Generates a {@code token} implementation constructor.
	 * If the {@code token} is an {@code interface}, returns an empty {@code String}.
	 * Otherwise, generates a constructor based on an arbitrary non-private constructor of {@code token}.
	 * The generated constructor immediately calls {@code super(...)}.
	 *
	 * @param token the type token
	 * @return a {@code String} representation of an arbitrary {@code token} implementation constructor,
	 * or an empty {@code String}, if the constructor is not required
	 * @throws ImplerException if a constructor is required, but no non-private constructors of {@code token} are found
	 * @see #getConstructor(Constructor)
	 */
	private String getConstructor(Class<?> token) throws ImplerException {
		if (token.isInterface()) {
			return "";
		}
		return Arrays.stream(token.getDeclaredConstructors())
		             .filter(c -> !Modifier.isPrivate(c.getModifiers()))
		             .findAny()
		             .map(this::getConstructor)
		             .orElseThrow(() -> new ImplerException("Cannot implement abstract class with no public constructors"));
	}
	
	/**
	 * Generates a {@code String} representation of the provided {@code constructor}.
	 * Uses {@link Constructor#getModifiers()}, class name, {@link Constructor#getParameters()},
	 * {@link Constructor#getExceptionTypes()}, and body to generate the constructor.
	 * The default body immediately calls {@code super(...)}.
	 *
	 * @param constructor the constructor
	 * @return a {@code String} representation of the provided {@code constructor}
	 * @see #getParameters(Parameter[], boolean)
	 * @see #getFunction(int, Parameter[], Class[], String, String...)
	 */
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
	
	/**
	 * Generates {@code token} method implementations.
	 * Finds all {@code abstract} methods, and provides a default implementation using {@link #getMethod(Method)}.
	 *
	 * @param token the type token
	 * @return a {@code String} representation of generated {@code abstract} methods
	 * @see #getAbstractMethods(Class)
	 * @see #getMethod(Method)
	 */
	private String getMethods(Class<?> token) {
		List<Method> abstractMethods = getAbstractMethods(token);
		return joinBlocks(abstractMethods, this::getMethod);
	}
	
	/**
	 * Generates {@code method} default implementation.
	 * Uses {@link Method#getModifiers()}, {@link Method#getReturnType()}, {@link Method#getName()},
	 * {@link Method#getParameters()}, and {@link #getDefaultValue(Class)} to generate the method.
	 *
	 * @param method the method
	 * @return a {@code String} representation of the specified {@code method} implementation
	 * @see #getDefaultValue(Class)
	 * @see #getFunction(int, Parameter[], String, String...)
	 */
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
	
	/**
	 * Returns a {@code List} of {@code abstract} methods of {@code token}. Scans the {@code token},
	 * and its superclasses for available {@code abstract} methods.
	 * Uses a {@code Set} of {@code MethodSignature} objects to avoid duplicate methods.
	 *
	 * @param token the type token
	 * @return a {@code List} of available {@code abstract} methods
	 * @see MethodSignature
	 * @see #processMethods(Method[], Set, Set)
	 */
	private List<Method> getAbstractMethods(Class<?> token) {
		Set<MethodSignature> abstractMethods = new HashSet<>();
		Set<MethodSignature> finalMethods = new HashSet<>();
		processMethods(token.getMethods(), abstractMethods, finalMethods);
		while (token != null) {
			processMethods(token.getDeclaredMethods(), abstractMethods, finalMethods);
			token = token.getSuperclass();
		}
		abstractMethods.removeAll(finalMethods);
		return abstractMethods.stream()
		                      .map(MethodSignature::getMethod)
		                      .collect(Collectors.toList());
	}
	
	/**
	 * Generates a {@code String} representation of a function with no checked exceptions, using its modifiers,
	 * parameters, body, and additional qualifiers.
	 *
	 * @param modifiers  a set of modifiers
	 * @param parameters an array of {@code Parameter} objects, describing the function parameters
	 * @param body       the function body
	 * @param tokens     additional qualifiers inserted before the parameters.
	 *                   Usually contain the name and/or return type of the function.
	 * @return a {@code String} representation of the function with no checked exceptions
	 * @see #getFunction(int, Parameter[], Class[], String, String...)
	 */
	private String getFunction(int modifiers, Parameter[] parameters, String body, String... tokens) {
		return getFunction(modifiers, parameters, new Class<?>[0], body, tokens);
	}
	
	/**
	 * Generates a {@code String} representation of a function, using
	 * {@link #getFunctionDeclaration(int, Parameter[], Class[], String...)} and {@code body}.
	 *
	 * @param modifiers      a set of modifiers
	 * @param parameters     an array of {@code Parameter} objects, describing the function parameters
	 * @param exceptionTypes an array of {@code Class} objects, describing exception types, thrown by the function
	 * @param body           the function body
	 * @param tokens         additional qualifiers inserted before the parameters.
	 *                       Usually contain the name and/or return type of the function
	 * @return a {@code String} representation of the function
	 * @see #getFunctionDeclaration(int, Parameter[], Class[], String...)
	 * @see #declAndBody(String, String)
	 */
	private String getFunction(int modifiers, Parameter[] parameters, Class<?>[] exceptionTypes,
	                           String body, String... tokens) {
		String declaration = getFunctionDeclaration(modifiers, parameters, exceptionTypes, tokens);
		return declAndBody(declaration, body);
	}
	
	/**
	 * Generates a {@code String} representation of a function declaration, using {@link #getAccessModifier(int)},
	 * additional qualifiers specified by {@code tokens}, {@link #getParameters(Parameter[], boolean)},
	 * <p>
	 * and {@link #getExceptions(Class[])}.
	 *
	 * @param modifiers      a set of modifiers
	 * @param parameters     an array of {@code Parameter} objects, describing the function parameters
	 * @param exceptionTypes an array of {@code Class} objects, describing exception types, thrown by the function
	 * @param tokens         additional qualifiers inserted before the parameters.
	 *                       Usually contains the name and/or return type of the function
	 * @return a {@code String} representation of the function declaration
	 * @see #getParameters(Parameter[], boolean)
	 * @see #getExceptions(Class[])
	 */
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
	
	/**
	 * Returns a comma separated list of {@code parameters}.
	 * Supports both typed mode for function parameters, and non-typed mode for function calls.
	 *
	 * @param parameters an array of {@code Parameter} objects
	 * @param typed      true, if the resulting parameter list should be typed
	 * @return a {@code String} representation of comma separated {@code parameters}
	 * @see Parameter
	 * @see #join(Object[], Function, String)
	 */
	private String getParameters(Parameter[] parameters, boolean typed) {
		return join(parameters,
				typed ? p -> String.join(SPACE, p.getType().getCanonicalName(), p.getName())
						: Parameter::getName, COMMA + SPACE);
	}
	
	/**
	 * Returns a comma separated list of {@code exceptions}.
	 *
	 * @param exceptionTypes an array of exception types
	 * @return a {@code String} representation of comma separated {@code exceptions}
	 * @see #join(Object[], Function, String)
	 */
	private String getExceptions(Class<?>[] exceptionTypes) {
		return join(exceptionTypes, Class::getCanonicalName, COMMA + SPACE);
	}
	
	/**
	 * Processes an array of methods, populating the provided {@code abstractMethods} and populating
	 * the provided {@code finalMethods} set.
	 * Filters the {@code methods} array, leaving only abstract methods, and
	 * wraps them in a {@code MethodSignature} object. Same for final methods.
	 *
	 * @param methods         an array of methods to be processed
	 * @param abstractMethods a {@code Set} of {@code MethodSignature} objects to be populated by abstract methods
	 * @param finalMethods    a {@code Set} of {@code MethodSignature} objects to be populated by final methods
	 * @see Method
	 * @see MethodSignature
	 */
	private void processMethods(Method[] methods, Set<MethodSignature> abstractMethods, Set<MethodSignature> finalMethods) {
		Arrays.stream(methods)
		      .filter(m -> Modifier.isFinal(m.getModifiers()))
		      .map(MethodSignature::new)
		      .collect(Collectors.toCollection(() -> finalMethods));
		Arrays.stream(methods)
		      .filter(m -> Modifier.isAbstract(m.getModifiers()))
		      .map(MethodSignature::new)
		      .collect(Collectors.toCollection(() -> abstractMethods));
	}
	
	/**
	 * Returns the full path to {@code .java} file considering the {@code root} and {@code token} package.
	 *
	 * @param root  the base directory
	 * @param token the type token to create implementation for
	 * @return the full path to {@code token} implementation file
	 */
	private Path getFullPath(Path root, Class<?> token) {
		return root.resolve(Path.of(getPackageDir(token, File.separator), getClassName(token) + JAVA));
	}
	
	/**
	 * Returns a directory, corresponding to the package of the provided {@code token}.
	 *
	 * @param token the type token
	 * @return a {@code String} representation of the resulting directory
	 */
	private String getPackageDir(Class<?> token, String separator) {
		return token.getPackageName().replace(".", separator);
	}
	
	/**
	 * Returns the name of {@code token} implementation class.
	 *
	 * @param token the type token
	 * @return name of the class, implementing {@code token}
	 */
	private String getClassName(Class<?> token) {
		return token.getSimpleName() + CLASS_SUFFIX;
	}
	
	/**
	 * Returns the default return value of an object of type {@code token}.
	 * <ul>
	 *     <li>For non-primitive types the value is {@code null}</li>
	 *     <li>For numerical primitives the value is {@code 0}</li>
	 *     <li>For {@code boolean} the value is {@code false}</li>
	 *     <li>For {@code void} the value is an empty {@code String}</li>
	 * </ul>
	 *
	 * @param token the type token
	 * @return default return value for provided {@code token}
	 */
	private String getDefaultValue(Class<?> token) {
		if (!token.isPrimitive()) {
			return "null";
		}
		if (token == boolean.class) {
			return "false";
		}
		if (token == void.class) {
			return "";
		}
		return "0";
	}
	
	/**
	 * Returns the access modifier based on provided {@code mod}.
	 *
	 * @param mod a set of modifiers
	 * @return a modifier, representing the access modifier of {@code mod} alone
	 */
	private int getAccessModifier(int mod) {
		if (Modifier.isPrivate(mod)) {
			return Modifier.PRIVATE;
		}
		if (Modifier.isProtected(mod)) {
			return Modifier.PROTECTED;
		}
		if (Modifier.isPublic(mod)) {
			return Modifier.PUBLIC;
		}
		return 0;
	}
	
	/**
	 * Returns {@code true} if the provided {@code token} cannot be implemented.
	 * This is the case if the token is:
	 * <ul>
	 *     <li>a primitive type</li>
	 *     <li>an array type</li>
	 *     <li>{@code final}</li>
	 *     <li>{@code private}</li>
	 *     <li>an {@code Enum}</li>
	 * </ul>
	 *
	 * @param token the type token
	 * @return {@code true}, if the provided {@code token} cannot be implemented
	 */
	private boolean unimplementable(Class<?> token) {
		int modifiers = token.getModifiers();
		return token.isPrimitive()
				|| token.isArray()
				|| Modifier.isFinal(modifiers)
				|| Modifier.isPrivate(modifiers)
				|| token == Enum.class;
	}
	
	/**
	 * Combines {@code declaration} and {@code body} into properly formatted {@code String}.
	 * <p>
	 * Resulting formatting:
	 * <pre>
	 *  declaration {
	 *      body
	 *  }
	 * </pre>
	 *
	 * @param declaration the declaration used when formatting
	 * @param body        the body used when formatting
	 * @return the resulting formatted {@code String}
	 */
	private String declAndBody(String declaration, String body) {
		return String.format("%s {%s%s%s}", declaration, LINE_SEP, tabbed(body, 1), LINE_SEP);
	}
	
	/**
	 * Tabulates every line of {@code body} exactly {@code amount} number of times.
	 *
	 * @param body   the {@code String} to tabulate
	 * @param amount number, specifying the amount of tabs
	 * @return the tabbed {@code String}
	 */
	private String tabbed(String body, int amount) {
		var tabs = TAB.repeat(amount);
		return body.lines()
		           .map(l -> tabs + l)
		           .collect(Collectors.joining(LINE_SEP));
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by one blank line.
	 *
	 * @param blocks the {@code String}s to separate
	 * @return the separated {@code String}
	 * @see #joinBlocks(Object[], Function)
	 */
	private String joinBlocks(String... blocks) {
		return joinBlocks(blocks, Function.identity());
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by one blank line,
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks   the objects to separate
	 * @param toString mapping function for provided objects
	 * @param <T>      the type of elements in {@code blocks} array
	 * @return the separated {@code String}
	 * @see #joinBlocks(Collection, Function)
	 */
	private <T> String joinBlocks(T[] blocks, Function<T, String> toString) {
		return joinBlocks(Arrays.asList(blocks), toString);
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by one blank line,
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks   the objects to separate
	 * @param toString mapping function for provided objects
	 * @param <T>      the type of elements in {@code blocks} collection
	 * @return the separated {@code String}
	 */
	private <T> String joinBlocks(Collection<T> blocks, Function<T, String> toString) {
		return join(blocks, toString, LINE_SEP.repeat(2));
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by {@code separator},
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks    the objects to separate
	 * @param toString  mapping function for provided objects
	 * @param separator the separator used between each element
	 * @param <T>       the type of elements in {@code blocks} array
	 * @return the separated {@code String}
	 * @see #join(Collection, Function, String)
	 */
	private <T> String join(T[] blocks, Function<T, String> toString, String separator) {
		return join(Arrays.asList(blocks), toString, separator);
	}
	
	/**
	 * Returns a {@code String}, with {@code blocks} separated by {@code separator},
	 * applying the {@code toString} function beforehand.
	 *
	 * @param blocks    the objects to separate
	 * @param toString  mapping function for provided objects
	 * @param separator the separator used between each element
	 * @param <T>       the type of elements in {@code blocks} collection
	 * @return the separated {@code String}
	 * @see Collectors#joining(CharSequence)
	 */
	private <T> String join(Collection<T> blocks, Function<T, String> toString, String separator) {
		return blocks.stream().map(toString).collect(Collectors.joining(separator));
	}
	
	/**
	 * Encodes the provided {@code String}, escaping all unicode characters in {@code \\u} notation.
	 *
	 * @param s the {@code String} to be encoded
	 * @return the encoded {@code String}
	 */
	private String encode(String s) {
		StringBuilder sb = new StringBuilder();
		char[] charArray = s.toCharArray();
		for (char c : charArray) {
			if (c < 128) {
				sb.append(c);
			} else {
				sb.append("\\u").append(String.format("%04x", (int) c));
			}
		}
		return sb.toString();
	}
	
	/**
	 * A wrapper class for {@code Method} with custom hash. Contains a {@code Method} object and
	 * provides {@link #equals(Object)} and {@link #hashCode()} implementations based on {@link Method#getName()},
	 * and {@link Method#getParameterTypes()}, ignoring {@link Method#getDeclaringClass()}.
	 *
	 * @see Method#equals(Object)
	 * @see Method#hashCode()
	 */
	private static class MethodSignature {
		
		/**
		 * The wrapped {@code Method} object.
		 */
		private final Method method;
		
		/**
		 * Wrapper constructor. Creates a new {@code MethodSignature} instance, wrapping the provided {@code method}.
		 *
		 * @param method the method to be wrapped
		 */
		public MethodSignature(Method method) {
			this.method = method;
		}
		
		/**
		 * Getter method for wrapped {@code Method} object.
		 *
		 * @return the wrapped {@link MethodSignature#method}
		 */
		public Method getMethod() {
			return method;
		}
		
		/**
		 * Compares this {@code MethodSignature} against the specified object. Returns
		 * true if the objects are the same. Two {@code MethodSignatures} are the same if
		 * their wrapped {@link MethodSignature#method} objects have the same names and parameter types.
		 *
		 * @param obj the reference object with which to compare
		 * @return {@code true} if this object is the same as the {@code obj} argument, {@code false} otherwise
		 * @see Method#getName()
		 * @see Method#getParameterTypes()
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			if (!(obj instanceof MethodSignature)) {
				return false;
			}
			MethodSignature other = (MethodSignature) obj;
			return method.getName().equals(other.method.getName())
					&& Arrays.equals(method.getParameterTypes(), other.method.getParameterTypes());
		}
		
		/**
		 * Returns a hashcode for this {@code MethodSignature}. The hashcode is computed
		 * using the hashcodes for the wrapped {@link MethodSignature#method} name and parameter types.
		 *
		 * @return a hash code value for this object.
		 * @see Objects#hashCode(Object)
		 * @see Arrays#hashCode(Object[])
		 */
		@Override
		public int hashCode() {
			return Objects.hash(method.getName(), Arrays.hashCode(method.getParameterTypes()));
		}
	}
}
