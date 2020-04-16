package parser.expression;

public class And extends AbstractBinaryOperator {
	
	public And(Expression first, Expression second) {
		super(first, second);
	}
	
	@Override
	protected String getOperation() {
		return "&";
	}
	
}
