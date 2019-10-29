package generic.expression.operation.type;

import generic.expression.exceptions.OverflowException;

public class IntegerOperation implements Operation<Integer> {
	
	@Override
	public Integer add(Integer a, Integer b) throws OverflowException {
		return a + b;
	}
	
	@Override
	public Integer subtract(Integer a, Integer b) throws OverflowException {
		return a - b;
	}
	
	@Override
	public Integer multiply(Integer a, Integer b) throws OverflowException {
		return a * b;
	}
	
	@Override
	public Integer divide(Integer a, Integer b) throws OverflowException {
		return a / b;
	}
	
	@Override
	public Integer negate(Integer a) throws OverflowException {
		return -a;
	}
	
	@Override
	public Integer abs(Integer a) throws OverflowException {
		return Math.abs(a);
	}
	
	@Override
	public Integer square(Integer a) throws OverflowException {
		return multiply(a, a);
	}
	
	@Override
	public Integer mod(Integer a, Integer b) throws OverflowException {
		return a % b;
	}
	
	@Override
	public Integer parseNumber(String number) throws NumberFormatException {
			return Integer.parseInt(number);
	}
}
