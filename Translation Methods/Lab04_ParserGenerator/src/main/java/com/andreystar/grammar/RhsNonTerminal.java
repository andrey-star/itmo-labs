package com.andreystar.grammar;

import java.util.List;

public class RhsNonTerminal extends RhsAtom {
	
	private final List<String> args;
	
	public RhsNonTerminal(String name, List<String> args, String code) {
		super(name, code);
		this.args = args;
	}
	
	public List<String> getArgs() {
		return args;
	}
	
}
