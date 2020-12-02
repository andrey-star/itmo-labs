package expression.operation.checked;

import expression.AbstractUnaryOperator;
import expression.TripleExpression;
import expression.exceptions.IntegerOverflowException;

public class CheckedNegate extends AbstractUnaryOperator {
	
	public CheckedNegate(TripleExpression expression) {
		super(expression);
	}
	
	private void check(int x) throws IntegerOverflowException {
		if (x == Integer.MIN_VALUE) {
			throw new IntegerOverflowException();
		}
	}
	
	@Override
	protected int evaluateImpl(int x) throws IntegerOverflowException {
		check(x);
		return -x;
	}
	
}