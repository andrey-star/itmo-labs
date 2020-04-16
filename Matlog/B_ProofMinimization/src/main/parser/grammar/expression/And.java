package main.parser.grammar.expression;

public class And extends AbstractBinaryOperator {
	
	public And(Expression first, Expression second) {
		super(first, second);
	}
	
	@Override
	public String getOperator() {
		return "&";
	}
	
}
