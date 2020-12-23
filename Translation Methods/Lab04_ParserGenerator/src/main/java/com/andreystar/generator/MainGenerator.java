package com.andreystar.generator;

import com.andreystar.grammar.Grammar;
import com.andreystar.grammar.LexerRule;
import com.andreystar.util.StringUtils;

import java.util.Locale;
import java.util.stream.Collectors;

public class MainGenerator extends Generator {
	
	public MainGenerator(Grammar grammar) {
		super(grammar);
	}
	
	@Override
	public String getGeneratorName() {
		return "Main";
	}
	
	@Override
	public String generate() {
		StringBuilder res = new StringBuilder();
		res.append(generatePackage()).append("\n");
		res.append(generateImports()).append("\n");
		res.append(generateClassHeader()).append(" {\n\n");
		res.append(generateMainMethod()).append("\n");
		res.append("}\n");
		return StringUtils.formatCode(res.toString());
	}
	
	private String generateImports() {
		return """
				import java.io.IOException;
				import java.io.PrintWriter;
				import java.nio.file.Files;
				import java.nio.file.InvalidPathException;
				import java.nio.file.Path;
				import java.util.Objects;
				""";
	}
	
	private String generateClassHeader() {
		return "public class %s%s"
				.formatted(grammar.getName(), getGeneratorName());
	}
	
	private String generateMainMethod() {
		return """
				public static void main(String[] args) throws IOException {
					if (args == null || args.length != 1) {
						throw new IllegalArgumentException("Usage: %1$sMain <input file>");
					}
					try {
						Path inputFile = Path.of(Objects.requireNonNull(args[0]));
						if (!Files.isRegularFile(inputFile)) {
							throw new IllegalArgumentException("Expected the input to be a file");
						}
						String inputFileName = inputFile.getFileName().toString().split("\\\\.")[0];
						var %2$s = new %1$sParser(new %1$sLexer(Files.readString(inputFile))).%2$s();
						System.out.println(%2$s.%4$s);
					    var gv = new %1$sGV();
						gv.generateImage(gv.getGraph(%2$s._node), inputFile.getParent(), inputFileName);
						System.out.println("Tree generated at '%%s\\\\%%s.pdf'"
								.formatted(inputFile.getParent().toString(), inputFileName));
					} catch (InvalidPathException e) {
				 		System.err.println("Invalid input file path");
				 	}
				}
				""".formatted(grammar.getName(), grammar.getStart(), grammar.getName().toLowerCase(), grammar.getReturnAttribute());
	}
	
}
