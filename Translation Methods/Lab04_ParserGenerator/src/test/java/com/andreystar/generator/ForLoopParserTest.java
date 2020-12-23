package com.andreystar.generator;

import forloop.ForLoopLexer;
import forloop.ForLoopParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ForLoopParserTest {
	
	@Test
	public void test() {
		test("for (int i = 0; i < 10; i++)");
		test("for (   int i   = 0   ; i <   10   ; i   ++)", "for (int i = 0; i < 10; i++)");
		test("for (int abc = 0; def >= -10; ghi--)");
		test("for(int i=0; i<10;i++)", "for (int i = 0; i < 10; i++)");
		test("for (int i = 0; -15 <= b; i++)");
		test("for (int i = 0; -15 <= b; --i)");
		test("for (int i = 0; -15 == b; --i)");
	}
	
	@Test
	public void testLexerError() {
		assertLexerError("!");
		assertLexerError("$ab");
	}
	
	@Test
	public void testParserError() {
		assertParserError("for");
		assertParserError("for (");
		assertParserError("for (int");
		assertParserError("for (int i");
		assertParserError("for (int i =");
		assertParserError("for (int i = 0");
		assertParserError("for (int i = 0;");
		assertParserError("for (int i = 0; i");
		assertParserError("for (int i = 0; i <=");
		assertParserError("for (int i = 0; i <= 10");
		assertParserError("for (int i = 0; i <= 10;");
		assertParserError("for (int i = 0; i <= 10; i");
		assertParserError("for (int i = 0; i <= 10; i++");
		assertParserError("for int i = 0; i <= 10; i++");
		assertParserError("for (int i = 0; i = 10; i++)");
		assertParserError("for (int i == 0; i <= 10; i++)");
		assertParserError("for (int i = 0; i <= 10; ++i++)");
	}
	
	private void test(String input) {
		test(input, input);
	}
	
	private void test(String input, String expected) {
		assertEquals(expected, parse(input));
	}
	
	private void assertLexerError(String input) {
		assertThrows(ForLoopLexer.UnmatchedPatternException.class, () -> parse(input));
	}
	
	private void assertParserError(String input) {
		assertThrows(ForLoopParser.UnexpectedTokenException.class, () -> parse(input));
	}
	
	private String parse(String input) {
		return new ForLoopParser(new ForLoopLexer(input)).S().val;
	}
	
}