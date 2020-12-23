package com.andreystar.grammar;

public class RhsEpsilon extends RhsAtom {
	
	public RhsEpsilon() {
		this("");
	}
	
	public RhsEpsilon(String code) {
		super("_EPS", code);
	}
	
}
