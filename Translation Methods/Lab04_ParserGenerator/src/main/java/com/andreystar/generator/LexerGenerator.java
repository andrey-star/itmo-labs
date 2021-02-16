package com.andreystar.generator;

import com.andreystar.grammar.Grammar;
import com.andreystar.grammar.LexerRule;
import com.andreystar.util.StringUtils;

import java.util.stream.Collectors;

public class LexerGenerator extends Generator {
	
	public LexerGenerator(Grammar grammar) {
		super(grammar);
	}
	
	@Override
	public String getGeneratorName() {
		return "Lexer";
	}
	
	@Override
	public String generate() {
		StringBuilder res = new StringBuilder();
		res.append(generatePackage()).append("\n");
		res.append(generateImport()).append("\n");
		res.append(generateClassHeader()).append(" {\n\n");
		res.append(generateFieldsAndConstructor()).append("\n");
		res.append(generateMethods()).append("\n");
		res.append(generateUnmatchedPatternExceptionClass()).append("\n");
		res.append("}\n");
		return StringUtils.formatCode(res.toString());
	}
	
	private String generateImport() {
		return """
				import java.util.Iterator;
				import java.util.LinkedHashMap;
				import java.util.Map;
				import java.util.regex.Matcher;
				import java.util.regex.Pattern;
				""";
	}
	
	private String generateClassHeader() {
		return "public class %1$s%2$s implements Iterator<%1$sToken>, Iterable<%1$sToken>"
				.formatted(grammar.getName(), getGeneratorName());
	}
	
	private String generateFieldsAndConstructor() {
		return """
				private final Map<%1$sToken.Type, Pattern> tokenPatterns = Map.ofEntries(
				%3$s
				);
				private final Pattern skip = %4$s;
				private final Matcher matcher = skip.matcher("");
				
				private int curStart = 0;
				private int curEnd = 0;
				private boolean hasNext = true;
				
				private final String input;
				
				public %1$s%2$s(String input) {
				this.input = input;
				}
				""".formatted(grammar.getName(), getGeneratorName(), generateTokenPatterns(), generateSkipPattern());
	}
	
	private String generateSkipPattern() {
		for (LexerRule lexerRule : grammar.getLexerRules()) {
			if (lexerRule.name().equals("_SKIP")) {
				return "Pattern.compile(%s)".formatted(lexerRule.regex());
			}
		}
		return "Pattern.compile(\"\")";
	}
	
	private String generateTokenPatterns() {
		return grammar.getLexerRules().stream()
				.filter(rule -> !rule.name().equals("_SKIP"))
				.map(rule -> "Map.entry(%sToken.Type.%s, Pattern.compile(%s))"
						.formatted(grammar.getName(), rule.name(), rule.regex()))
				.collect(Collectors.joining(",\n"));
	}
	
	private String generateMethods() {
		StringBuilder res = new StringBuilder();
		res.append(generateIterable()).append("\n");
		res.append(generateIterator()).append("\n");
		res.append("""
				private boolean matchLookingAt() {
					if (matcher.lookingAt()) {
						curStart = curEnd;
						curEnd = curStart + matcher.end();
						matcher.reset(input.substring(curEnd));
						return true;
					}
					return false;
				}
				
				public int curPos() {
					return curEnd;
				}
				""");
		return res.toString();
	}
	
	private String generateIterator() {
		return """
				@Override
				public Iterator<%sToken> iterator() {
					return this;
				}
				""".formatted(grammar.getName());
	}
	
	private String generateIterable() {
		return generateNextMethod() + "\n" + generateHasNextMethod();
	}
	
	private String generateNextMethod() {
		return """
				@Override
					public %1$sToken next() {
						curStart = curEnd;
						matcher.usePattern(skip);
						matcher.reset(input.substring(curStart));
						matchLookingAt();
						for (var tokenType : %1$sToken.Type.values()) {
							if (tokenType == %1$sToken.Type._END) {
				  				continue;
				  			}
							matcher.usePattern(tokenPatterns.get(tokenType));
							if (matchLookingAt()) {
								return new %1$sToken(tokenType, input.substring(curStart, curEnd));
							}
						}
						if (curEnd != input.length()) {
							throw new UnmatchedPatternException(input, curEnd);
						}
						hasNext = false;
						return new %1$sToken(%1$sToken.Type._END, null);
					}
					""".formatted(grammar.getName());
	}
	
	private String generateHasNextMethod() {
		return """
				@Override
				public boolean hasNext() {
					return hasNext;
				}
				""";
	}
	
	private String generateUnmatchedPatternExceptionClass() {
		return """
				public static class UnmatchedPatternException extends RuntimeException {
				 	public UnmatchedPatternException(String text, int errIndex) {
							super("Unmatched pattern in text: '" + text + "' at index " + (errIndex + 1));
						}
					}
				""";
	}
	
}
