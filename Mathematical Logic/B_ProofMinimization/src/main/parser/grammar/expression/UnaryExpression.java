package main.parser.grammar.expression;

public interface UnaryExpression extends Expression {
	
	Expression getExpression();
	
	String getOperator();
	
}
