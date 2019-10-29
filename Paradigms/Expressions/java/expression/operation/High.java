package expression.operation;

import expression.AbstractUnaryOperator;
import expression.TripleExpression;

public class High extends AbstractUnaryOperator {
	
	public High(TripleExpression expression) {
		super(expression);
	}
	
	@Override
	protected int evaluateImpl(int expression) {
		return Integer.highestOneBit(expression);
	}
	
}