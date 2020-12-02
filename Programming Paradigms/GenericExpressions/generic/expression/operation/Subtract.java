package generic.expression.operation;

import generic.expression.AbstractBinaryOperator;
import generic.expression.TripleExpression;
import generic.expression.exceptions.OverflowException;
import generic.expression.operation.type.Operation;

public class Subtract<T> extends AbstractBinaryOperator<T> {

	public Subtract(TripleExpression<T> first, TripleExpression<T> second, Operation<T> operation) {
		super(first, second, operation);
	}
	
	@Override
	protected T evaluateImpl(T first, T second) throws OverflowException {
		return operation.subtract(first, second);
	}
	
	
}
