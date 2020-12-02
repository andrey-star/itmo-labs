package generic.expression.operation.type;


import generic.expression.exceptions.OverflowException;

public class ByteOperation implements Operation<Byte> {
	
	@Override
	public Byte add(Byte a, Byte b) {
		return (byte) (a + b);
	}
	
	@Override
	public Byte subtract(Byte a, Byte b) {
		return (byte) (a - b);
	}
	
	@Override
	public Byte multiply(Byte a, Byte b) {
		return (byte) (a * b);
	}
	
	@Override
	public Byte divide(Byte a, Byte b) {
		return (byte) (a / b);
	}
	
	@Override
	public Byte negate(Byte a) {
		return (byte) (-a);
	}
	
	@Override
	public Byte abs(Byte a) {
		return (byte) Math.abs(a);
	}
	
	@Override
	public Byte square(Byte a) {
		return multiply(a, a);
	}
	
	@Override
	public Byte mod(Byte a, Byte b) {
		return (byte) (a % b);
	}
	
	@Override
	public Byte parseNumber(String number) throws NumberFormatException {
		return (byte) Integer.parseInt(number);
	}
}
