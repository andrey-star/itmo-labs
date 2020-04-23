package parser.expression;

public abstract class AbstractUnaryOperator implements Expression {
	
	final Expression expression;
	
	public AbstractUnaryOperator(Expression expression) {
		this.expression = expression;
	}
	
	@Override
	public String toString() {
		return String.format("(%s%s)", getOperation(), expression);
	}
	
	protected abstract String getOperation();
	
}
