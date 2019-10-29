package generic.expression.operation.type;


import generic.expression.exceptions.OverflowException;

import java.math.BigInteger;

public class BigIntegerOperation implements Operation<BigInteger> {
	
	@Override
	public BigInteger add(BigInteger a, BigInteger b) {
		return a.add(b);
	}
	
	@Override
	public BigInteger subtract(BigInteger a, BigInteger b) {
		return a.subtract(b);
	}
	
	@Override
	public BigInteger multiply(BigInteger a, BigInteger b) {
		return a.multiply(b);
	}
	
	@Override
	public BigInteger divide(BigInteger a, BigInteger b) throws OverflowException {
		if (b.equals(BigInteger.ZERO)) {
			throw new OverflowException();
		}
		return a.divide(b);
	}
	
	@Override
	public BigInteger negate(BigInteger a) {
		return a.negate();
	}
	
	@Override
	public BigInteger abs(BigInteger a) {
		return a.abs();
	}
	
	@Override
	public BigInteger square(BigInteger a) {
		return multiply(a, a);
	}
	
	@Override
	public BigInteger mod(BigInteger a, BigInteger b) {
		return a.mod(b);
	}
	
	@Override
	public BigInteger parseNumber(String number) throws NumberFormatException {
			return new BigInteger(number);
	}
}
