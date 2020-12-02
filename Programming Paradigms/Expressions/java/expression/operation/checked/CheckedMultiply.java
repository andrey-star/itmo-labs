package expression.operation.checked;

import expression.AbstractBinaryOperator;
import expression.TripleExpression;
import expression.exceptions.IntegerOverflowException;

public class CheckedMultiply extends AbstractBinaryOperator {
	
	public CheckedMultiply(TripleExpression first, TripleExpression second) {
		super(first, second);
	}
	
	private void check(int x, int y) throws IntegerOverflowException {
		if (x > 0 && y > 0 && Integer.MAX_VALUE / y < x) {
			throw new IntegerOverflowException();
		}
		if (x > 0 && y < 0 && Integer.MIN_VALUE / x > y) {
			throw new IntegerOverflowException();
		}
		if (x < 0 && y < 0 && Integer.MAX_VALUE / x > y) {
			throw new IntegerOverflowException();
		}
		if (x < 0 && y > 0 && Integer.MIN_VALUE / y > x) {
			throw new IntegerOverflowException();
		}
	}
	
	@Override
	protected int evaluateImpl(int first, int second) throws IntegerOverflowException {
		check(first, second);
		return first * second;
	}
	
}
