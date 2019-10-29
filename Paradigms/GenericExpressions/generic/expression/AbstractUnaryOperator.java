package generic.expression;

import generic.expression.exceptions.EvaluatingException;
import generic.expression.operation.type.Operation;

public abstract class AbstractUnaryOperator<T> implements TripleExpression<T> {
	
	final TripleExpression<T> expression;
	protected final Operation<T> operation;
	
	public AbstractUnaryOperator(TripleExpression<T> expression, Operation<T> operation) {
		this.expression = expression;
		this.operation = operation;
	}
	
	@Override
	public T evaluate(T x, T y, T z) throws EvaluatingException {
		return evaluateImpl(expression.evaluate(x, y ,z));
	}
	
	protected abstract T evaluateImpl(T expression) throws EvaluatingException;
	
}
