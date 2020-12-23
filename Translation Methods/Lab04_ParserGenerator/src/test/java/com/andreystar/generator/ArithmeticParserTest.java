package com.andreystar.generator;

import arithmetic.ArithmeticLexer;
import arithmetic.ArithmeticParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ArithmeticParserTest {

	@Test
	public void testValue() {
		test(10, "10");
		test(-10, "-10");
		test(30, "10 + 20");
		test(200, "10 * 20");
		test(-10, "10 - 20");
		test(2, "20 / 10");
		test(-10, "10 + (15 / 5 - 8) * -4 / -1");
		test(-10, "10    + (   15 /   5 - 8) *    -4 /    -1");
		test(-5, "1 - 1 - 1 - 1 - 1 - 1 - 1");
		test(2, "sqrt(4)");
		test(6, "sqrt(4) + pow(2, 2)");
		test(4, "pow(sqrt(4), 2)");
		test(2, "pow(sqrt(4), 2) / 2");
	}

	@Test
	public void testParserError() {
		assertUTE("10 10");
		assertUTE("+");
		assertUTE("-+");
		assertUTE("10-");
		assertUTE("(10*10");
		assertUTE("()");
	}

	private void test(int expected, String input) {
		assertEquals(expected, parse(input));
	}

	private void assertUPE(String input) {
		assertThrows(ArithmeticLexer.UnmatchedPatternException.class, () -> parse(input));
	}

	private void assertUTE(String input) {
		assertThrows(ArithmeticParser.UnexpectedTokenException.class, () -> parse(input));
	}

	private int parse(String input) {
		return new ArithmeticParser(new ArithmeticLexer(input)).E().val;
	}

}