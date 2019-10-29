package generic.expression.operation.type;

import generic.expression.exceptions.OverflowException;

public interface Operation<T> {
	T add(T a, T b) throws OverflowException;
	T subtract(T a, T b) throws OverflowException;
	T multiply(T a, T b) throws OverflowException;
	T divide(T a, T b) throws OverflowException;
	T negate(T a) throws OverflowException;
	T abs(T a) throws OverflowException;
	T square(T a) throws OverflowException;
	T mod(T a, T b) throws OverflowException;
	T parseNumber(String number) throws NumberFormatException;
}
