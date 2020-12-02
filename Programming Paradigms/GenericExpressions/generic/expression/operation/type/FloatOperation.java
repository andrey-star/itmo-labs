package generic.expression.operation.type;


import generic.expression.exceptions.OverflowException;

public class FloatOperation implements Operation<Float> {
	
	@Override
	public Float add(Float a, Float b) {
		return a + b;
	}
	
	@Override
	public Float subtract(Float a, Float b) {
		return a - b;
	}
	
	@Override
	public Float multiply(Float a, Float b) {
		return a * b;
	}
	
	@Override
	public Float divide(Float a, Float b) {
		return a / b;
	}
	
	@Override
	public Float negate(Float a) {
		return -a;
	}
	
	@Override
	public Float abs(Float a) {
		return Math.abs(a);
	}
	
	@Override
	public Float square(Float a) {
		return multiply(a, a);
	}
	
	@Override
	public Float mod(Float a, Float b) {
		return a % b;
	}
	
	@Override
	public Float parseNumber(String number) throws NumberFormatException {
			return Float.parseFloat(number);
	}
}
