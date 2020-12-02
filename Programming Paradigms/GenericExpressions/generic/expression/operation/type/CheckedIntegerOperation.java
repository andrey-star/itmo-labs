package generic.expression.operation.type;

import generic.expression.exceptions.OverflowException;

public class CheckedIntegerOperation extends IntegerOperation {
	
	@Override
	public Integer add(Integer a, Integer b) throws OverflowException {
		checkAdd(a, b);
		return super.add(a, b);
	}
	
	@Override
	public Integer subtract(Integer a, Integer b) throws OverflowException {
		checkSubtract(a, b);
		return super.subtract(a, b);
	}
	
	@Override
	public Integer multiply(Integer a, Integer b) throws OverflowException {
		checkMultiply(a, b);
		return super.multiply(a, b);
	}
	
	@Override
	public Integer divide(Integer a, Integer b) throws OverflowException {
		checkDivide(a, b);
		return super.divide(a, b);
	}
	
	@Override
	public Integer negate(Integer a) throws OverflowException {
		checkNegate(a);
		return super.negate(a);
	}
	
	@Override
	public Integer square(Integer a) throws OverflowException {
		return multiply(a, a);
	}
	
	@Override
	public Integer abs(Integer a) throws OverflowException {
		checkAbs(a);
		return Math.abs(a);
	}
	
	@Override
	public Integer mod(Integer a, Integer b) throws OverflowException {
		checkMod(a, b);
		return super.mod(a, b);
	}
	
	private static void checkAdd(int x, int y) throws OverflowException {
		int sum = x + y;
		if (((x ^ sum) & (y ^ sum)) < 0) {
			throw new OverflowException();
		}
	}
	
	private void checkSubtract(int x, int y) throws OverflowException {
		int diff = x - y;
		if (((x ^ y) & (x ^ diff)) < 0) {
			throw new OverflowException();
		}
	}
	
	private void checkMultiply(int x, int y) throws OverflowException {
		if (x > 0 && y > 0 && Integer.MAX_VALUE / y < x) {
			throw new OverflowException();
		}
		if (x > 0 && y < 0 && Integer.MIN_VALUE / x > y) {
			throw new OverflowException();
		}
		if (x < 0 && y < 0 && Integer.MAX_VALUE / x > y) {
			throw new OverflowException();
		}
		if (x < 0 && y > 0 && Integer.MIN_VALUE / y > x) {
			throw new OverflowException();
		}
	}
	
	private void checkDivide(int x, int y) throws OverflowException {
		if (y == 0) {
			throw new OverflowException();
		} else if (y == -1 && x == Integer.MIN_VALUE) {
			throw new OverflowException();
		}
	}
	
	private void checkNegate(int x) throws OverflowException {
		if (x == Integer.MIN_VALUE) {
			throw new OverflowException();
		}
	}
	
	private void checkAbs(Integer a) throws OverflowException {
		if (a == Integer.MIN_VALUE) {
			throw new OverflowException();
		}
	}
	
	private void checkMod(Integer a, Integer b) throws OverflowException {
		if (b == 0) {
			throw new OverflowException();
		}
	}
}
