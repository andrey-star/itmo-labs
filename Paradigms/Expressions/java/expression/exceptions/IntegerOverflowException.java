package expression.exceptions;

public class IntegerOverflowException extends EvaluatingException {
	public IntegerOverflowException() {
		super("integer overflow");
	}
}
