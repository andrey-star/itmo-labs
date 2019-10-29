package generic.expression;

import generic.expression.exceptions.EvaluatingException;
import generic.expression.operation.type.Operation;

public abstract class AbstractBinaryOperator<T> implements TripleExpression<T> {
	
	private final TripleExpression<T> first;
	private final TripleExpression<T> second;
	protected final Operation<T> operation;
	
	public AbstractBinaryOperator(TripleExpression<T> first, TripleExpression<T> second, Operation<T> operation) {
		this.first = first;
		this.second = second;
		this.operation = operation;
	}
	
	@Override
	public T evaluate(T x, T y, T z) throws EvaluatingException {
		return evaluateImpl(first.evaluate(x, y ,z), second.evaluate(x, y, z));
	}
	
	protected abstract T evaluateImpl(T first, T second) throws EvaluatingException;
	
}
