package main.parser.grammar.expression;

public class Impl extends AbstractBinaryOperator {
	
	public Impl(Expression first, Expression second) {
		super(first, second);
	}
	
	@Override
	public String getOperator() {
		return "->";
	}
	
}
