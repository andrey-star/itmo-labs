package expression;

public class Const implements TripleExpression {
	private final double value;
	
	public Const(double value) {
		this.value = value;
	}
	
	@Override
	public int evaluate(int x, int y, int z) {
		return (int) value;
	}
	
	@Override
	public String toString() {
		return "" + (int) value;
	}
}
