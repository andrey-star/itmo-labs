package main.parser.grammar.expression;

import java.util.HashMap;
import java.util.Map;

public class Variables {
	
	public static final Map<String, Variable> vars = new HashMap<>();
	
	public static Variable getVariable(String name) {
		return vars.computeIfAbsent(name, Variable::new);
	}
	
}
