package main.parser.grammar;

import main.parser.grammar.expression.Expression;

import java.util.List;
import java.util.stream.Collectors;

public class Statement {
	
	private final List<Expression> hypothesis;
	private final Expression expression;
	
	public Statement(List<Expression> hypothesis, Expression expression) {
		this.hypothesis = hypothesis;
		this.expression = expression;
	}
	
	@Override
	public String toString() {
		return hypothesis.stream().map(Expression::toString).collect(Collectors.joining(","))
				+ "|-" + expression;
	}
	
	public List<Expression> getHypothesis() {
		return hypothesis;
	}
	
	public Expression getExpression() {
		return expression;
	}
}
