package main.parser.grammar.expression;

import java.util.Objects;

public abstract class AbstractUnaryOperator implements UnaryExpression {
	
	final Expression expression;
	
	public AbstractUnaryOperator(Expression expression) {
		this.expression = expression;
	}
	
	@Override
	public String toString() {
		return String.format("%s%s", getOperator(), expression);
	}
	
	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (!(o instanceof AbstractUnaryOperator)) {
			return false;
		}
		AbstractUnaryOperator that = (AbstractUnaryOperator) o;
		return getOperator().equals(that.getOperator()) &&
				Objects.equals(expression, that.expression);
	}
	
	
	@Override
	public Expression getExpression() {
		return expression;
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(getOperator(), expression);
	}
}
