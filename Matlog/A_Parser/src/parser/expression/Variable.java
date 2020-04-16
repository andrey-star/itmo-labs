package parser.expression;

public class Variable implements Expression {
	
	private final String name;
	
	public Variable(String name) {
		this.name = name;
	}
	
	@Override
	public String toString() {
		return name;
	}
}
