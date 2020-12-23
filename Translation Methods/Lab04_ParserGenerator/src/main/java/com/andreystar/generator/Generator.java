package com.andreystar.generator;

import com.andreystar.grammar.Grammar;

public abstract class Generator {
	
	protected final Grammar grammar;
	
	protected Generator(Grammar grammar) {
		this.grammar = grammar;
	}
	
	public String getFileName() {
		return grammar.getName() + getGeneratorName() + ".java";
	}
	
	public abstract String generate();
	
	protected String generatePackage() {
		return """
				package %s;
				""".formatted(grammar.getName().toLowerCase());
	}
	
	public abstract String getGeneratorName();
}
