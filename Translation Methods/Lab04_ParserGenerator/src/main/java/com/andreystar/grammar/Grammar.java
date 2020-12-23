package com.andreystar.grammar;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Grammar {
	
	private static final String _EPS = "_EPS";
	private static final String END = "_END";
	
	private final String name;
	private final String imports;
	private final String macros;
	private final List<LexerRule> lexerRules;
	private final List<ParserRule> parserRules;
	private final String start;
	private final String returnAttribute;
	private Map<String, Set<String>> first;
	private Map<String, Set<String>> follow;
	
	public Grammar(String name, String imports, String macros, List<LexerRule> lexerRules, List<ParserRule> parserRules, String start, String returnAttribute) {
		this.name = name;
		this.imports = imports == null ? "" : imports.substring(1, imports.length() - 1);
		this.macros = macros == null ? "" : macros;
		this.lexerRules = lexerRules;
		this.parserRules = parserRules;
		this.start = start;
		this.returnAttribute = returnAttribute;
		constructFirst();
		constructFollow();
	}
	
	private void constructFirst() {
		first = parserRules.stream()
				.map(ParserRule::name)
				.collect(Collectors.toMap(Function.identity(), s -> new HashSet<>()));
		boolean changed = true;
		while (changed) {
			changed = false;
			for (ParserRule parserRule : parserRules) {
				for (RuleRhs ruleRhs : parserRule.rhs()) {
					changed |= first.get(parserRule.name()).addAll(getFirst(ruleRhs));
				}
			}
		}
	}
	
	private Set<String> getFirst(RuleRhs ruleRhs) {
		RhsAtom ruleAtom = ruleRhs.atoms().get(0);
		if (ruleAtom instanceof RhsNonTerminal ruleNt) {
			return first.get(ruleNt.getName());
		}
		return Set.of(ruleAtom.getName());
	}
	
	public Set<String> getModifiedFirst(String ruleName, RuleRhs ruleRhs) {
		Set<String> first = getFirst(ruleRhs);
		Set<String> res = new HashSet<>(first);
		if (res.remove(_EPS)) {
			res.addAll(follow.get(ruleName));
		}
		return res;
	}
	
	private void constructFollow() {
		follow = parserRules.stream()
				.map(ParserRule::name)
				.collect(Collectors.toMap(Function.identity(), s -> new HashSet<>()));
		follow.get(start).add(END);
		boolean changed = true;
		while (changed) {
			changed = false;
			for (ParserRule parserRule : parserRules) {
				for (RuleRhs ruleRhs : parserRule.rhs()) {
					List<RhsAtom> atoms = ruleRhs.atoms();
					for (int i = 0; i < atoms.size(); i++) {
						RhsAtom rule = atoms.get(i);
						if (!(rule instanceof RhsNonTerminal)) {
							continue;
						}
						List<RhsAtom> rightAtoms = atoms.subList(i + 1, atoms.size());
						if (rightAtoms.isEmpty()) {
							rightAtoms = List.of(new RhsEpsilon());
						}
						RuleRhs right = new RuleRhs(rightAtoms);
						Set<String> rightFirst = new HashSet<>(getFirst(right));
						boolean hadEps = rightFirst.remove(_EPS);
						changed |= follow.get(rule.getName()).addAll(rightFirst);
						if (hadEps) {
							changed |= follow.get(rule.getName()).addAll(follow.get(parserRule.name()));
						}
					}
				}
			}
		}
	}
	
	public String getName() {
		return name;
	}
	
	public String getImports() {
		return imports;
	}
	
	public String getMacros() {
		return macros;
	}
	
	public List<LexerRule> getLexerRules() {
		return lexerRules;
	}
	
	public List<ParserRule> getParserRules() {
		return parserRules;
	}
	
	public String getStart() {
		return start;
	}
	
	public String getReturnAttribute() {
		return returnAttribute;
	}
}
