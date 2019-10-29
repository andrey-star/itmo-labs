package expression;

import expression.exceptions.EvaluatingException;

public abstract class AbstractUnaryOperator implements TripleExpression {
	
	final TripleExpression expression;
	
	public AbstractUnaryOperator(TripleExpression expression) {
		this.expression = expression;
	}
	
	@Override
	public int evaluate(int x, int y, int z) throws EvaluatingException {
		return evaluateImpl(expression.evaluate(x, y ,z));
	}
	
	protected abstract int evaluateImpl(int expression) throws EvaluatingException;
	
}
