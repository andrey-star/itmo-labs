package expression.operation;

import expression.AbstractUnaryOperator;
import expression.TripleExpression;

public class Low extends AbstractUnaryOperator {
	
	public Low(TripleExpression expression) {
		super(expression);
	}
	
	@Override
	protected int evaluateImpl(int expression) {
		return Integer.lowestOneBit(expression);
	}
	
}