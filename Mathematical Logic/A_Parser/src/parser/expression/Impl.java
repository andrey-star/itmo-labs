package parser.expression;

public class Impl extends AbstractBinaryOperator {
	
	public Impl(Expression first, Expression second) {
		super(first, second);
	}
	
	@Override
	protected String getOperation() {
		return "->";
	}
	
}
