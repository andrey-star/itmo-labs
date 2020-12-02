package generic.expression.operation.type;


import generic.expression.exceptions.OverflowException;

public class DoubleOperation implements Operation<Double> {
	
	@Override
	public Double add(Double a, Double b) {
		return a + b;
	}
	
	@Override
	public Double subtract(Double a, Double b) {
		return a - b;
	}
	
	@Override
	public Double multiply(Double a, Double b) {
		return a * b;
	}
	
	@Override
	public Double divide(Double a, Double b) {
		return a / b;
	}
	
	@Override
	public Double negate(Double a) {
		return -a;
	}
	
	@Override
	public Double abs(Double a) {
		return Math.abs(a);
	}
	
	@Override
	public Double square(Double a) {
		return multiply(a, a);
	}
	
	@Override
	public Double mod(Double a, Double b) {
		return a % b;
	}
	
	@Override
	public Double parseNumber(String number) throws NumberFormatException {
			return Double.parseDouble(number);
	}
}
