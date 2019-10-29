package generic.expression.parser;

import generic.expression.TripleExpression;
import generic.expression.exceptions.ParsingException;
import generic.expression.operation.type.Operation;
import generic.expression.operation.*;

import java.util.Map;
import java.util.Set;

public class ExpressionParser<T> implements Parser<T> {
	
	private static final Map<String, BinaryOperation> binaryOperations = Map.of(
			"+", BinaryOperation.ADD,
			"-", BinaryOperation.SUB,
			"*", BinaryOperation.MUL,
			"/", BinaryOperation.DIV,
			"mod", BinaryOperation.MOD
	);
	private static final Set<String> variables = Set.of(
			"x",
			"y",
			"z"
	);
	private final char END = '\0';
	private final Operation<T> operation;
	private String expression;
	private int curIndex = 0;
	private BinaryOperation curBinaryOperation;
	
	public ExpressionParser(Operation<T> operation) {
		this.operation = operation;
	}
	
	private TripleExpression<T> unary() throws ParsingException {
		curBinaryOperation = BinaryOperation.NONE;
		skipWhitespaces();
		if (Character.isDigit(getChar()) || (test('-') && Character.isDigit(getChar(curIndex + 1)))) {
			String numberStr = readDigits();
			try {
				return new Const<>(operation.parseNumber(numberStr));
			} catch (NumberFormatException e) {
				curIndex -= numberStr.length();
				throw error("Unable to read number '%s'", numberStr);
			}
		}
		if (testNext('-')) {
			return new Negate<>(unary(), operation);
		}
		if (testNext('(')) {
			int index = curIndex - 1;
			TripleExpression<T> inner = addSub();
			if (testNext(')')) {
				return inner;
			}
			curIndex = index;
			throw error("Mismatched parenthesis", getChar());
		}
		if (!test(END)) {
			String identifier = parseIdentifier();
			if (variables.contains(identifier)) {
				return new Variable<>(identifier);
			}
			switch (identifier) {
				case "abs":
					return new Abs<>(unary(), operation);
				case "square":
					return new Square<>(unary(), operation);
			}
			curIndex -= identifier.length();
			throw error("Expected value, found '%s'", identifier);
		}
		throw error("Expected value, found EOF");
	}
	
	private TripleExpression<T> mulDivMod() throws ParsingException {
		var left = unary();
		while (true) {
			curBinaryOperation = parseOperation();
			switch (curBinaryOperation) {
				case MUL:
					left = new Multiply<>(left, unary(), operation);
					break;
				case DIV:
					left = new Divide<>(left, unary(), operation);
					break;
				case MOD:
					left = new Mod<>(left, unary(), operation);
					break;
				default:
					return left;
			}
		}
	}
	
	private TripleExpression<T> addSub() throws ParsingException {
		var left = mulDivMod();
		while (true) {
			switch (curBinaryOperation) {
				case ADD:
					left = new Add<>(left, mulDivMod(), operation);
					break;
				case SUB:
					left = new Subtract<>(left, mulDivMod(), operation);
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
	public TripleExpression<T> parse(String expression) throws ParsingException {
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
