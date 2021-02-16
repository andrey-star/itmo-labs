package com.andreystar;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.*;

public class ParserUtils {
	
	public static void parseFromFile(String inputFile, String outputFile) throws IOException {
		String s = parseFromStream(new FileInputStream(inputFile));
		try (PrintWriter out = new PrintWriter(new File(outputFile))) {
			out.print(s);
		}
	}
	
	public static void parseFromFile(String inputFile) throws IOException {
		System.out.println(parseFromStream(new FileInputStream(inputFile)));
	}
	
	public static String parseFromString(String inputString) throws IOException {
		return parseFromStream(new ByteArrayInputStream(inputString.getBytes()));
	}
	
	public static String parseFromStream(InputStream is) throws IOException {
		return getParser(is).program().value;
	}
	
	public static String tree(String inputString) throws IOException {
		com.andreystar.Python2CParser parser = getParser(new ByteArrayInputStream(inputString.getBytes()));
		ParseTree tree = parser.program();
		return tree.toStringTree(parser);
	}
	
	private static com.andreystar.Python2CParser getParser(InputStream is) throws IOException {
		ANTLRInputStream input = new ANTLRInputStream(is);
		com.andreystar.Python2CLexer lexer = new com.andreystar.Python2CLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		return new com.andreystar.Python2CParser(tokens);
	}
	
}
