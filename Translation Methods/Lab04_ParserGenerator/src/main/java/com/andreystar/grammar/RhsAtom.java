package com.andreystar.grammar;

public abstract class RhsAtom {
	
	private final String name;
	private final String code;
	
	protected RhsAtom(String name, String code) {
		this.name = name;
		this.code = code;
	}
	
	public String getName() {
		return name;
	}
	
	public String getCode() {
		return code == null ? "" : code;
	}
}
