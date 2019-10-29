package generic.expression.operation;

import generic.expression.AbstractUnaryOperator;
import generic.expression.TripleExpression;
import generic.expression.exceptions.OverflowException;
import generic.expression.operation.type.Operation;

public class Abs<T> extends AbstractUnaryOperator<T> {

	public Abs(TripleExpression<T> first, Operation<T> operation) {
		super(first, operation);
	}
	
	@Override
	protected T evaluateImpl(T first) throws OverflowException {
		return operation.abs(first);
	}
	
	
}
