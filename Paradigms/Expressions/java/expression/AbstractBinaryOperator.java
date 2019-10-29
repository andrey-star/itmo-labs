package expression;

import expression.exceptions.EvaluatingException;

public abstract class AbstractBinaryOperator implements TripleExpression {
	
	final TripleExpression first;
	final TripleExpression second;
	
	public  AbstractBinaryOperator(TripleExpression first, TripleExpression second) {
		this.first = first;
		this.second = second;
	}
	
	@Override
	public int evaluate(int x, int y, int z) throws EvaluatingException {
		return evaluateImpl(first.evaluate(x, y ,z), second.evaluate(x, y, z));
	}
	
	protected abstract int evaluateImpl(int first, int second) throws EvaluatingException;
	
}
