package token;

import java.text.ParseException;

public class LexicalAnalyzer {
	
	private static final char END = '\0';
	String source;
	int curPos;
	char curChar;
	Token curToken;
	String curValue;
	
	public LexicalAnalyzer(String source) throws ParseException {
		this.source = source + END;
		curPos = -1;
		nextChar();
	}
	
	private boolean isBlank(char c) {
		return c == ' ' || c == '\r' || c == '\n' || c == '\t';
	}
	
	public void nextChar() throws ParseException {
		curPos++;
		if (curPos >= source.length()) {
			throw new ParseException(source, curPos);
		}
		curChar = source.charAt(curPos);
	}
	
	public void nextToken() throws ParseException {
		while (isBlank(curChar)) {
			nextChar();
		}
		switch (curChar) {
			case '(':
				nextChar();
				curToken = Token.OPEN_BRACKET;
				break;
			case ')':
				nextChar();
				curToken = Token.CLOSE_BRACKET;
				break;
			case ';':
				nextChar();
				curToken = Token.SEMICOLON;
				break;
			case '=':
				nextChar();
				curToken = Token.ASSIGN;
				if (curChar == '=') {
					nextChar();
					curToken = Token.EQ;
				}
				break;
			case '<':
				nextChar();
				curToken = Token.LT;
				if (curChar == '=') {
					nextChar();
					curToken = Token.LE;
				}
				break;
			case '>':
				nextChar();
				curToken = Token.GT;
				if (curChar == '=') {
					nextChar();
					curToken = Token.GE;
				}
				break;
			case '+':
				nextChar();
				if (curChar == '+') {
					nextChar();
					curToken = Token.INC;
				} else {
					throw new ParseException("Expected '+' after '+', but found: " + curChar, curPos);
				}
				break;
			case '-':
				if (Character.isDigit(source.charAt(curPos + 1))) {
					curValue = parseNumber();
					curToken = Token.NUMBER;
				} else {
					nextChar();
					if (curChar == '-') {
						nextChar();
						curToken = Token.DEC;
					} else {
						throw new ParseException("Expected '-' after '-', but found: " + curChar, curPos);
					}
				}
				break;
			case END:
				curToken = Token.END;
				break;
			default:
				curToken = getLongToken();
				break;
		}
	}
	
	private Token getLongToken() throws ParseException {
		if (Character.isDigit(curChar)) {
			curValue = parseNumber();
			return Token.NUMBER;
		} else if (Character.isLetter(curChar) || curChar == '_') {
			curValue = parseIdentifier();
			if (curValue.equals("for")) {
				return Token.FOR;
			}
			return Token.VAR;
		}
		throw new ParseException("Expected letter, but found: " + curChar, curPos);
	}
	
	
	private String parseIdentifier() throws ParseException {
		StringBuilder sb = new StringBuilder();
		sb.append(curChar);
		nextChar();
		while (Character.isLetterOrDigit(curChar) || curChar == '_') {
			sb.append(curChar);
			nextChar();
		}
		return sb.toString();
	}
	
	private String parseNumber() throws ParseException {
		String number = readDigits();
		try {
			Integer.parseInt(number);
			return number;
		} catch (final NumberFormatException e) {
			curPos -= number.length();
			throw new ParseException("Unable to read number '" + number + "'", curPos);
		}
	}
	
	private String readDigits() throws ParseException {
		StringBuilder sb = new StringBuilder();
		do {
			sb.append(curChar);
			nextChar();
		} while (Character.isDigit(curChar));
		return sb.toString();
	}
	
	
	public Token curToken() {
		return curToken;
	}
	
	public int curPos() {
		return curPos;
	}
	
	public String curValue() {
		return curValue;
	}
}
