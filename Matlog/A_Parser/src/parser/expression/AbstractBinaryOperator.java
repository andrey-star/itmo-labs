package parser.expression;

public abstract class AbstractBinaryOperator implements Expression {
	
	final Expression first;
	final Expression second;
	
	public  AbstractBinaryOperator(Expression first, Expression second) {
		this.first = first;
		this.second = second;
	}
	
	@Override
	public String toString() {
		return String.format("(%s,%s,%s)", getOperation(), first, second);
	}
	
	protected abstract String getOperation();
	
}
