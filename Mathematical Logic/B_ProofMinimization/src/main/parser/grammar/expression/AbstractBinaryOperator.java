package main.parser.grammar.expression;

import java.util.Objects;

public abstract class AbstractBinaryOperator implements BinaryExpression {
	
	final Expression first;
	final Expression second;
	
	public AbstractBinaryOperator(Expression first, Expression second) {
		this.first = first;
		this.second = second;
	}
	
	@Override
	public String toString() {
		return String.format("(%s %s %s)", first, getOperator(), second);
	}
	
	@Override
	public Expression getFirst() {
		return first;
	}
	
	@Override
	public Expression getSecond() {
		return second;
	}
	
	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (!(o instanceof AbstractBinaryOperator)) {
			return false;
		}
		AbstractBinaryOperator that = (AbstractBinaryOperator) o;
		return getOperator().equals(that.getOperator()) &&
				Objects.equals(first, that.first) &&
				Objects.equals(second, that.second);
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(first, getOperator(), second);
	}
}
