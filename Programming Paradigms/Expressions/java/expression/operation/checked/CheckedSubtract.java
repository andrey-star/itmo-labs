package expression.operation.checked;

import expression.AbstractBinaryOperator;
import expression.TripleExpression;
import expression.exceptions.IntegerOverflowException;

public class CheckedSubtract extends AbstractBinaryOperator {
	
	public CheckedSubtract(TripleExpression first, TripleExpression second) {
		super(first, second);
	}
	
	private void check(int x, int y) throws IntegerOverflowException {
		int diff = x - y;
		if (((x ^ y) & (x ^ diff)) < 0) {
			throw new IntegerOverflowException();
		}
	}
	
	@Override
	protected int evaluateImpl(int first, int second) throws IntegerOverflowException {
		check(first, second);
		return first - second;
	}
	
}
