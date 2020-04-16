package main.parser.grammar.expression;

public interface BinaryExpression extends Expression {
	
	Expression getFirst();
	
	Expression getSecond();
	
	String getOperator();
	
}
