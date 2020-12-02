package expression.operation.checked;

import expression.AbstractBinaryOperator;
import expression.TripleExpression;
import expression.exceptions.IntegerOverflowException;

public class CheckedAdd extends AbstractBinaryOperator {

	public CheckedAdd(TripleExpression first, TripleExpression second) {
		super(first, second);
	}
	
	private static void check(int x, int y) throws IntegerOverflowException {
		int sum = x + y;
		if (((x ^ sum) & (y ^ sum)) < 0) {
			throw new IntegerOverflowException();
		}
	}
	
	@Override
	protected int evaluateImpl(int first, int second) throws IntegerOverflowException {
		check(first, second);
		return first + second;
	}
	
}
