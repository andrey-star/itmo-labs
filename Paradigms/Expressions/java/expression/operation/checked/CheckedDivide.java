package expression.operation.checked;

import expression.AbstractBinaryOperator;
import expression.TripleExpression;
import expression.exceptions.IntegerOverflowException;

public class CheckedDivide extends AbstractBinaryOperator {
	
	public CheckedDivide(TripleExpression first, TripleExpression second) {
		super(first, second);
	}
	
	private void check(int x, int y) throws IntegerOverflowException {
		if (y == 0) {
			throw new IntegerOverflowException();
		} else if (y == -1 && x == Integer.MIN_VALUE) {
			throw new IntegerOverflowException();
		}
	}
	
	@Override
	protected int evaluateImpl(int first, int second) throws IntegerOverflowException {
		check(first, second);
		return first / second;
	}
	
}
