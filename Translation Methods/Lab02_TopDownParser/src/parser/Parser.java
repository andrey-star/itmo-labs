package parser;

import token.LexicalAnalyzer;
import token.Token;

import java.text.ParseException;

public class Parser {
	
	// S - for loop
	// C - condition
	// H - change
	
	// S -> for (VAR VAR = NUM; C; H)
	// C -> VAR CMP NUM
	// C -> NUM CMP VAR
	// H -> VAR SHIFT
	// H -> SHIFT VAR
	// VAR = [a-zA_Z_][a-zA_Z0-9_]*
	// NUM = [\-\+]?[0-9]+ (int)
	// CMP = < | >= | <= | >
	// SHIFT = ++ | --
	
	LexicalAnalyzer lex;
	
	public Node parse(String input) throws ParseException {
		lex = new LexicalAnalyzer(input);
		lex.nextToken();
		return S();
	}
	
	private Node S() throws ParseException {
		if (lex.curToken() == Token.FOR) {
			lex.nextToken();
			if (lex.curToken() != Token.OPEN_BRACKET) {
				throw new ParseException("Expected '(' at position", lex.curPos());
			}
			lex.nextToken();
			if (lex.curToken() != Token.VAR) {
				throw new ParseException("Expected variable at position", lex.curPos());
			}
			lex.nextToken();
			if (lex.curToken() != Token.VAR) {
				throw new ParseException("Expected variable at position", lex.curPos());
			}
			lex.nextToken();
			if (lex.curToken() != Token.ASSIGN) {
				throw new ParseException("Expected '=' at position", lex.curPos());
			}
			lex.nextToken();
			if (lex.curToken() != Token.NUMBER) {
				throw new ParseException("Expected number at position", lex.curPos());
			}
			lex.nextToken();
			if (lex.curToken() != Token.SEMICOLON) {
				throw new ParseException("Expected ';' at position", lex.curPos());
			}
			Node c = C();
			Node h = H();
			lex.nextToken();
			if (lex.curToken() != Token.CLOSE_BRACKET) {
				throw new ParseException("Expected ')' at position", lex.curPos());
			}
			lex.nextToken();
			if (lex.curToken() != Token.END) {
				throw new ParseException("Expected EOF at position", lex.curPos());
			}
			return new Node("S", new Node("for"), new Node("("),
					new Node("VAR"), new Node("VAR"), new Node("="), new Node("NUM"), new Node(";"),
					c, h, new Node(")")
			);
		}
		throw new ParseException("Unexpected token", lex.curPos());
	}
	
	private Node H() throws ParseException {
		lex.nextToken();
		switch (lex.curToken()) {
			case VAR:
				lex.nextToken();
				Token op = lex.curToken();
				if (op != Token.DEC && op != Token.INC) {
					throw new ParseException("Expected '++' or '--' at position", lex.curPos());
				}
				return new Node("H", new Node("VAR"), new Node("SHIFT"));
			case DEC:
			case INC:
				lex.nextToken();
				if (lex.curToken() != Token.VAR) {
					throw new ParseException("Expected variable at position", lex.curPos());
				}
				return new Node("H", new Node("SHIFT"), new Node("VAR"));
			default:
				throw new ParseException("Expected variable or shift at position", lex.curPos());
		}
	}
	
	private Node C() throws ParseException {
		lex.nextToken();
		switch (lex.curToken()) {
			case VAR: {
				lex.nextToken();
				Token cmp = lex.curToken();
				if (cmp != Token.LT && cmp != Token.GT && cmp != Token.LE && cmp != Token.GE && cmp != Token.EQ) {
					throw new ParseException("Expected '>/</>=/<=/==' at position", lex.curPos());
				}
				lex.nextToken();
				if (lex.curToken() != Token.NUMBER) {
					throw new ParseException("Expected number at position", lex.curPos());
				}
				lex.nextToken();
				if (lex.curToken() != Token.SEMICOLON) {
					throw new ParseException("Expected ';' at position", lex.curPos());
				}
				return new Node("C", new Node("VAR"), new Node("CMP"), new Node("NUM"), new Node(";"));
			}
			case NUMBER: {
				lex.nextToken();
				Token cmp = lex.curToken();
				if (cmp != Token.LT && cmp != Token.GT && cmp != Token.LE && cmp != Token.GE && cmp != Token.EQ) {
					throw new ParseException("Expected '>/</>=/<=/==' at position", lex.curPos());
				}
				lex.nextToken();
				if (lex.curToken() != Token.VAR) {
					throw new ParseException("Expected number at position", lex.curPos());
				}
				lex.nextToken();
				if (lex.curToken() != Token.SEMICOLON) {
					throw new ParseException("Expected ';' at position", lex.curPos());
				}
				return new Node("C", new Node("NUM"), new Node("CMP"), new Node("VAR"), new Node(";"));
			}
			default:
				throw new ParseException("Expected variable at position", lex.curPos());
		}
		
	}
	
}
