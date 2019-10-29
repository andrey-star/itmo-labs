package generic.expression.operation;

import generic.expression.AbstractUnaryOperator;
import generic.expression.TripleExpression;
import generic.expression.exceptions.OverflowException;
import generic.expression.operation.type.Operation;

public class Square<T> extends AbstractUnaryOperator<T> {

	public Square(TripleExpression<T> first, Operation<T> operation) {
		super(first, operation);
	}
	
	@Override
	protected T evaluateImpl(T first) throws OverflowException {
		return operation.square(first);
	}
	
	
}
