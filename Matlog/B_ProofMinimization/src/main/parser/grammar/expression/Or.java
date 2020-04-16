package main.parser.grammar.expression;

public class Or extends AbstractBinaryOperator {
	
	public Or(Expression first, Expression second) {
		super(first, second);
	}
	
	@Override
	public String getOperator() {
		return "|";
	}
	
}
