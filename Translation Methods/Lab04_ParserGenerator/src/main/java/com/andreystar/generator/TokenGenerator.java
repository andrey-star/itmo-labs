package com.andreystar.generator;

import com.andreystar.grammar.Grammar;
import com.andreystar.grammar.LexerRule;
import com.andreystar.util.StringUtils;

import java.util.stream.Collectors;

public class TokenGenerator extends Generator {
	
	public TokenGenerator(Grammar grammar) {
		super(grammar);
	}
	
	@Override
	public String getGeneratorName() {
		return "Token";
	}
	
	@Override
	public String generate() {
		StringBuilder res = new StringBuilder();
		res.append(generatePackage()).append("\n");
		res.append(generateClassHeader()).append(" {\n\n");
		res.append(generateTokenTypes()).append("\n");
		res.append("}\n");
		return StringUtils.formatCode(res.toString());
	}
	
	private String generateClassHeader() {
		return "public record %1$sToken(Type type, String text)"
				.formatted(grammar.getName());
	}
	
	private String generateTokenTypes() {
		return "public enum Type {\n" +
				grammar.getLexerRules().stream().map(LexerRule::name).filter(name -> !name.equals("_SKIP")).collect(Collectors.joining(", ")) + ", _END\n" +
				"}\n";
	}
	
}
