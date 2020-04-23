package main.parser.grammar.expression;

public class Not extends AbstractUnaryOperator {
	
	public Not(Expression expression) {
		super(expression);
	}
	
	@Override
	public String getOperator() {
		return "!";
	}
	
}
