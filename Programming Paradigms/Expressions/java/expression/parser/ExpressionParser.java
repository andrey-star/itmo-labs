package expression.parser;

import expression.*;
import expression.exceptions.*;
import expression.operation.High;
import expression.operation.Low;
import expression.operation.checked.*;

import java.lang.NumberFormatException;
import java.util.Map;
import java.util.Set;

public class ExpressionParser implements Parser {
	
	private static final Map<String, BinaryOperation> binaryOperations = Map.of(
			"+", BinaryOperation.ADD,
			"-", BinaryOperation.SUB,
			"*", BinaryOperation.MUL,
			"/", BinaryOperation.DIV
	);
	private static final Set<String> variables = Set.of(
			"x",
			"y",
			"z"
	);
	private final char END = '\0';
	private String expression;
	private int curIndex = 0;
	private BinaryOperation curBinaryOperation;
	
	private TripleExpression unary() throws ParsingException {
		curBinaryOperation = BinaryOperation.NONE;
		skipWhitespaces();
		if (Character.isDigit(getChar()) || (test('-') && Character.isDigit(getChar(curIndex + 1)))) {
			return new Const(parseNumber());
		}
		if (testNext('-')) {
			return new CheckedNegate(unary());
		}
		if (testNext('(')) {
			int index = curIndex - 1;
			TripleExpression inner = addSub();
			if (testNext(')')) {
				return inner;
			}
			curIndex = index;
			throw error("Mismatched parenthesis");
		}
		if (!test(END)) {
			String identifier = parseIdentifier();
			if (variables.contains(identifier)) {
				return new Variable(identifier);
			}
			switch (identifier) {
				case "high":
					return new High(unary());
				case "low":
					return new Low(unary());
			}
			curIndex -= identifier.length();
			throw error("Expected value, found '%s'", identifier);
		}
		throw error("Expected value, found EOF");
	}
	
	private TripleExpression mulDiv() throws ParsingException {
		TripleExpression left = unary();
		while (true) {
			curBinaryOperation = parseOperation();
			switch (curBinaryOperation) {
				case MUL:
					left = new CheckedMultiply(left, unary());
					break;
				case DIV:
					left = new CheckedDivide(left, unary());
					break;
				default:
					return left;
			}
		}
	}
	
	private TripleExpression addSub() throws ParsingException {
		var left = mulDiv();
		while (true) {
			switch (curBinaryOperation) {
				case ADD:
					left = new CheckedAdd(left, mulDiv());
					break;
				case SUB:
					left = new CheckedSubtract(left, mulDiv());
					break;
				default:
					return left;
			}
		}
	}
	
	private BinaryOperation parseOperation() throws ParsingException {
		skipWhitespaces();
		String operation = parseIdentifier();
		if (binaryOperations.containsKey(operation)) {
			return binaryOperations.get(operation);
		}
		curIndex -= operation.length();
		if (test(')')) {
			return BinaryOperation.CLOSE_BRACKET;
		}
		if (!test(END)) {
			throw error("Expected operation, found '%s'", getChar());
		}
		return BinaryOperation.NONE;
	}
	
	private String parseIdentifier() {
		final StringBuilder sb = new StringBuilder();
		if (!test(END)) {
			sb.append(getChar());
			if (Character.isLetter(getChar())) {
				while (Character.isLetterOrDigit(nextChar())) {
					sb.append(getChar());
				}
			} else {
				nextChar();
			}
		}
		return sb.toString();
	}
	
	private int parseNumber() throws ParsingException {
		String number = readDigits();
		try {
			return Integer.parseInt(number);
		} catch (final NumberFormatException e) {
			curIndex -= number.length();
			throw error("Unable to read number '%s'", number);
		}
	}
	
	private String readDigits() {
		StringBuilder sb = new StringBuilder();
		do {
			sb.append(getChar());
		} while (Character.isDigit(nextChar()));
		return sb.toString();
	}
	
	private void skipWhitespaces() {
		char c = expression.charAt(curIndex);
		while (Character.isWhitespace(c)) {
			curIndex++;
			c = expression.charAt(curIndex);
		}
	}
	
	private char nextChar() {
		return getChar(++curIndex);
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
	public TripleExpression parse(String expression) throws ParsingException {
		reset();
		this.expression = expression + END;
		var res = addSub();
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
