package parser.expression;

public class Or extends AbstractBinaryOperator {
	
	public Or(Expression first, Expression second) {
		super(first, second);
	}
	
	@Override
	protected String getOperation() {
		return "|";
	}
	
}
