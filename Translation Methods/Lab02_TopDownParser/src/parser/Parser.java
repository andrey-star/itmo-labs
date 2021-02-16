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
	
	public Tree parse(String input) throws ParseException {
		lex = new LexicalAnalyzer(input);
		lex.nextToken();
		return S();
	}
	
	private Tree S() throws ParseException {
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
			Tree c = C();
			Tree h = H();
			lex.nextToken();
			if (lex.curToken() != Token.CLOSE_BRACKET) {
				throw new ParseException("Expected ')' at position", lex.curPos());
			}
			lex.nextToken();
			if (lex.curToken() != Token.END) {
				throw new ParseException("Expected EOF at position", lex.curPos());
			}
			return new Tree("S", new Tree("for"), new Tree("("),
					new Tree("VAR"), new Tree("VAR"), new Tree("="), new Tree("NUM"), new Tree(";"),
					c, h, new Tree(")")
			);
		}
		throw new ParseException("Unexpected token", lex.curPos());
	}
	
	private Tree H() throws ParseException {
		lex.nextToken();
		switch (lex.curToken()) {
			case VAR:
				lex.nextToken();
				Token op = lex.curToken();
				if (op != Token.DEC && op != Token.INC) {
					throw new ParseException("Expected '++' or '--' at position", lex.curPos());
				}
				return new Tree("H", new Tree("VAR"), new Tree("SHIFT"));
			case DEC:
			case INC:
				lex.nextToken();
				if (lex.curToken() != Token.VAR) {
					throw new ParseException("Expected variable at position", lex.curPos());
				}
				return new Tree("H", new Tree("SHIFT"), new Tree("VAR"));
			default:
				throw new ParseException("Expected variable or shift at position", lex.curPos());
		}
	}
	
	private Tree C() throws ParseException {
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
				return new Tree("C", new Tree("VAR"), new Tree("CMP"), new Tree("NUM"), new Tree(";"));
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
				return new Tree("C", new Tree("NUM"), new Tree("CMP"), new Tree("VAR"), new Tree(";"));
			}
			default:
				throw new ParseException("Expected variable at position", lex.curPos());
		}
		
	}
	
}
