package parser;

import parser.exception.ParsingException;
import parser.expression.*;

import java.util.HashMap;
import java.util.Map;

public class MathLogicParser implements Parser {
	
	private static final Map<String, BinaryOperation> binaryOperations = new HashMap<>();
	
	
	static {
		binaryOperations.put("&", BinaryOperation.AND);
		binaryOperations.put("|", BinaryOperation.OR);
		binaryOperations.put("->", BinaryOperation.IMPL);
	}
	
	private final char END = '\0';
	private String expression;
	private int curIndex = 0;
	private BinaryOperation curBinaryOperation;
	
	private Expression unary() throws ParsingException {
		curBinaryOperation = BinaryOperation.NONE;
		if (testNext('!')) {
			return new Not(unary());
		}
		if (testNext('(')) {
			int index = curIndex - 1;
			Expression inner = impl();
			if (testNext(')')) {
				return inner;
			}
			curIndex = index;
			throw error("Mismatched parenthesis");
		}
		if (!test(END)) {
			String identifier = parseIdentifier();
			return Variables.getVariable(identifier);
		}
		throw error("Expected value, found EOF");
	}
	
	private Expression and() throws ParsingException {
		Expression left = unary();
		while (true) {
			curBinaryOperation = parseOperation();
			if (curBinaryOperation == BinaryOperation.AND) {
				left = new And(left, unary());
			} else {
				return left;
			}
		}
	}
	
	private Expression or() throws ParsingException {
		Expression left = and();
		while (true) {
			if (curBinaryOperation == BinaryOperation.OR) {
				left = new Or(left, and());
			} else {
				return left;
			}
		}
	}
	
	private Expression impl() throws ParsingException {
		Expression left = or();
		while (true) {
			if (curBinaryOperation == BinaryOperation.IMPL) {
				left = new Impl(left, impl());
			} else {
				return left;
			}
		}
	}
	
	private BinaryOperation parseOperation() {
		if (testNext('&')) {
			return binaryOperations.get("&");
		}
		if (testNext('|')) {
			return binaryOperations.get("|");
		}
		if (testNext('-') && testNext('>')) {
			return binaryOperations.get("->");
		}
		return BinaryOperation.NONE;
	}
	
	private String parseIdentifier() {
		final StringBuilder sb = new StringBuilder();
		if (!test(END)) {
			sb.append(getChar());
			if (Character.isLetter(getChar())) {
				while (fits(nextChar())) {
					sb.append(getChar());
				}
			} else {
				nextChar();
			}
		}
		return sb.toString();
	}
	
	private char nextChar() {
		return getChar(++curIndex);
	}
	
	private boolean fits(char c) {
		return Character.isLetterOrDigit(c) || c == '’' || (c == '\'') || (c == '`');
	}
	
	private boolean test(char c) {
		return getChar() == c;
	}
	
	private boolean testNext(char c) {
		boolean test = test(c);
		if (test) {
			curIndex++;
		}
		return test;
	}
	
	private char getChar() {
		return getChar(curIndex);
	}
	
	private char getChar(int index) {
		return expression.charAt(index);
	}
	
	private ParsingException error(final String format, final Object... args) {
		return new ParsingException(format, curIndex, args);
	}
	
	@Override
	public Expression parse(String expression) throws ParsingException {
		reset();
		this.expression = expression + END;
		Expression res = impl();
		if (!test(END)) {
			throw error("Mismatched parenthesis", curIndex);
		}
		return res;
	}
	
	private void reset() {
		curIndex = 0;
		curBinaryOperation = BinaryOperation.NONE;
	}
	
}
