package com.andreystar.util;

import com.andreystar.grammar.Grammar;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;

public class MetaGrammarParser {
	
	public static Grammar parseGrammarFromString(String inputString) throws IOException {
		return parseGrammarFromStream(new ByteArrayInputStream(inputString.getBytes()));
	}
	
	public static Grammar parseGrammarFromFile(Path grammarDef) throws IOException {
		return getParser(Files.newInputStream(grammarDef)).grammarDef().grammar;
	}
	
	private static Grammar parseGrammarFromStream(InputStream is) throws IOException {
		return getParser(is).grammarDef().grammar;
	}
	
	private static com.andreystar.MetaParser getParser(InputStream is) throws IOException {
		ANTLRInputStream input = new ANTLRInputStream(is);
		com.andreystar.MetaLexer lexer = new com.andreystar.MetaLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		return new com.andreystar.MetaParser(tokens);
	}
	
}
