package com.andreystar.generator;

import com.andreystar.util.StringUtils;
import com.andreystar.grammar.*;

import java.util.*;
import java.util.stream.Collectors;

public class ParserGenerator extends Generator {
	
	private final Map<String, List<TypedName>> ntReturns;
	
	public ParserGenerator(Grammar grammar) {
		super(grammar);
		this.ntReturns = grammar.getParserRules()
				.stream()
				.collect(Collectors.toMap(ParserRule::name, ParserRule::returns));
	}
	
	@Override
	public String getGeneratorName() {
		return "Parser";
	}
	
	@Override
	public String generate() {
		StringBuilder res = new StringBuilder();
		res.append(generatePackage()).append("\n");
		res.append(generateImports()).append("\n");
		res.append(generateClassHeader()).append(" {\n\n");
		res.append(generateMacros()).append("\n");
		res.append(generateFieldsAndConstructor()).append("\n");
		grammar.getParserRules().stream().map(this::generateParserRule).forEach(res::append);
		res.append(generateNodeClass()).append("\n");
		res.append(generateUnexpectedTokenExceptionClass()).append("\n");
		res.append("}\n");
		return StringUtils.formatCode(res.toString());
	}
	
	private String generateImports() {
			return """
				import java.util.List;
				import java.util.ArrayList;
				import java.util.stream.Collectors;
				%s
				""".formatted(grammar.getImports());
	}
	
	private String generateClassHeader() {
		return "public class " + grammar.getName() + getGeneratorName();
	}
	
	private String generateMacros() {
		return grammar.getMacros();
	}
	
	private String generateFieldsAndConstructor() {
		return """
				%1$sLexer lexer;
				%1$sToken curToken;
				
				public %1$s%2$s(%1$sLexer lexer) {
					this.lexer = lexer;
					curToken = this.lexer.next();
				}
				""".formatted(grammar.getName(), getGeneratorName());
	}
	
	private String generateParserRule(ParserRule parserRule) {
		String classs = generateReturnClass(parserRule);
		String method = generateMethod(parserRule);
		return classs + "\n" + method + "\n";
	}
	
	private String generateReturnClass(ParserRule parserRule) {
		StringBuilder res = new StringBuilder();
		res.append("public class ").append(parserRule.name()).append(" {\n");
		List<TypedName> fields = new ArrayList<>();
		fields.addAll(parserRule.params());
		fields.addAll(parserRule.returns());
		res.append("public Node _node = new Node(\"%s\");".formatted(parserRule.name())).append("\n");
		for (TypedName param : fields) {
			res.append("public ").append(param.type()).append(" ").append(param.name()).append(";\n");
		}
		res.append("""
				public String text() {
					return _node.toString();
				}
				""");
		res.append("}\n");
		return res.toString();
	}
	
	private String generateMethod(ParserRule parserRule) {
		String methodHeader = generateMethodHeader(parserRule);
		String methodBody = generateMethodBody(parserRule);
		return "%s {\n%s}\n".formatted(methodHeader, methodBody);
	}
	
	private String generateMethodHeader(ParserRule parserRule) {
		String res = "public %s %s(".formatted(parserRule.name(), parserRule.name());
		res += parserRule.params()
				.stream()
				.map(param -> param.type() + " " + param.name()).collect(Collectors.joining(", "));
		res += ")";
		return res;
	}
	
	private String generateMethodBody(ParserRule parserRule) {
		String name = parserRule.name();
		String retVal = name + "0";
		String retValInit = generateRetValInit(parserRule);
		String switchCase = generateSwitch(parserRule, retVal);
		return retValInit + "\n" + switchCase + "\n" + "return " + retVal + ";\n";
	}
	
	private String generateRetValInit(ParserRule parserRule) {
		String name = parserRule.name();
		String retVal = name + "0";
		StringBuilder res = new StringBuilder("var %s = new %s();\n".formatted(retVal, name));
		for (TypedName param : parserRule.params()) {
			res.append("%s.%s = %s;%n".formatted(retVal, param.name(), param.name()));
		}
		return res.toString();
	}
	
	private String generateSwitch(ParserRule parserRule, String retVal) {
		StringBuilder res = new StringBuilder();
		res.append("switch (curToken.type()) {\n");
		List<RuleRhs> ruleRhs = parserRule.rhs();
		for (RuleRhs rhs : ruleRhs) {
			String options = String.join(", ", grammar.getModifiedFirst(parserRule.name(), rhs));
			res.append("case ").append(options).append(" -> {\n");
			res.append(generateCaseBody(rhs, retVal));
			res.append("}\n");
		}
		res.append("default -> throw new UnexpectedTokenException(curToken, lexer.curPos());\n");
		res.append("}\n");
		return res.toString();
	}
	
	private String generateCaseBody(RuleRhs rhs, String retVal) {
		StringBuilder res = new StringBuilder();
		List<RhsAtom> atoms = rhs.atoms();
		for (int i = 0; i < atoms.size(); i++) {
			RhsAtom atom = atoms.get(i);
			if (atom instanceof RhsNonTerminal) {
				res.append("var %s%d = new %s();%n".formatted(atom.getName(), i + 1, atom.getName()));
			}
		}
		res.append("\n");
		for (int i = 0; i < atoms.size(); i++) {
			RhsAtom atom = atoms.get(i);
			if (atom instanceof RhsTerminal rhsTerm) {
				res.append(generateTerminal(rhsTerm, i + 1, retVal));
			} else if (atom instanceof RhsEpsilon rhsEps) {
				res.append(generateEpsilon(rhsEps, retVal)).append("\n");
			} else {
				res.append(generateNonTerminal((RhsNonTerminal) atom, i + 1, retVal));
			}
			res.append("\n");
		}
		return res.toString();
	}
	
	private String generateTerminal(RhsTerminal term, int index, String retVal) {
		return """
				if (curToken.type() != %1$sToken.Type.%2$s) {
					throw new UnexpectedTokenException(curToken, %1$sToken.Type.%2$s, lexer.curPos());
				}
				var %2$s%3$d = curToken;
				%4$s
				Node _%2$s%3$d = new Node(%1$sToken.Type.%2$s);
				_%2$s%3$d.children.add(new Node(%2$s%3$d.text()));
				%5$s._node.children.add(_%2$s%3$d);
				curToken = lexer.next();
				""".formatted(grammar.getName(), term.getName(), index, getCode(term), retVal);
	}
	
	private String generateEpsilon(RhsEpsilon eps, String retVal) {
		return getCode(eps) + "\n" + "%s._node.children.add(new Node(\"\"));".formatted(retVal) + "\n";
	}
	
	private String getCode(RhsAtom atom) {
		if (atom.getCode().startsWith("$") && atom.getCode().endsWith("$")) {
			return atom.getName();
		}
		return atom.getCode();
	}
	
	private String generateNonTerminal(RhsNonTerminal nterm, int index, String retVal) {
		StringBuilder res = new StringBuilder();
		String args = String.join(", ", nterm.getArgs());
		res.append("var _%1$s%2$d = %1$s(%3$s);%n".formatted(nterm.getName(), index, args));
		for (TypedName param : ntReturns.get(nterm.getName())) {
			res.append("%1$s%2$d.%3$s = _%1$s%2$d.%3$s;%n".formatted(nterm.getName(), index, param.name()));
		}
		res.append("%1$s%2$d.%3$s = _%1$s%2$d.%3$s;%n".formatted(nterm.getName(), index, "_node"));
		res.append(getCode(nterm)).append("\n");
		res.append("%s._node.children.add(%s%d._node);".formatted(retVal, nterm.getName(), index)).append("\n");
		return res.toString();
	}
	
	private String generateNodeClass() {
		return """
				public class Node {
					public String value;
					
					public List<Node> children;
					
					public Node(String value) {
						this.value = value;
						this.children = new ArrayList<>();
					}
					
					public Node(%sToken.Type terminal) {
						this(terminal.toString());
					}
					
					@Override
					public String toString() {
						if (children.isEmpty()) {
							return value;
						}
						return children.stream().map(Node::toString).collect(Collectors.joining(\" \"));
					}
					
				}
				""".formatted(grammar.getName());
	}
	
	private String generateUnexpectedTokenExceptionClass() {
		return """
				public static class UnexpectedTokenException extends RuntimeException {
					public UnexpectedTokenException(%sToken token, int errIndex) {
						super("Unexpected token: '" + token.type() + "' at index " + errIndex);
					}
					
					public UnexpectedTokenException(%1$sToken actual, %1$sToken.Type expected, int errIndex) {
						super("Expected token: '" + expected + "', but found '" + actual.type() + "' at index " + (errIndex + 1));
					}
				}
				
				""".formatted(grammar.getName());
	}
}
