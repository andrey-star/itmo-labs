package parser.expression;

public class Not extends AbstractUnaryOperator {
	
	public Not(Expression expression) {
		super(expression);
	}
	
	@Override
	protected String getOperation() {
		return "!";
	}
	
}
