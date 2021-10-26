package parser;

import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.junit.jupiter.api.Assertions.*;

class ParserTest {
	
	@Test
	public void test() {
		assertCorrect("for (int i = 0; i < 100; i++)");
		assertCorrect("for (a a = 100; a >= -100; a--)");
		assertCorrect("for (a a = -100; a <= -100; a++)");
		assertCorrect("for (int a = -100; a == -100; a++)");
		
		assertIncorrect("for");
		assertIncorrect("for ()");
		// no semicolon
		assertIncorrect("for (int i = 0 i < 100; i++)");
		assertIncorrect("for (int i = 0; i < 100 i++)");
		// no type
		assertIncorrect("for (a = 100; a < 100; a++)");
		// bad number
		assertIncorrect("for (int x = 100; x < --100; x++)");
		// bad cmp
		assertIncorrect("for (int x = 100; x x -100; x++)");
		// bad increment
		assertIncorrect("for (int x = 100; x < -100; x+++)");
		// odd semicolon
		assertIncorrect("for (int x = 100; x < -100; x++;)");
		// bad cmp
		assertIncorrect("for (int x = 100; x === -100; x++)");
	}
	
	@Test
	public void test_mod() {
		assertCorrect("for (int i = 0; i < 100; i++)");
		// reverse cmp
		assertCorrect("for (int i = 0; 100 < i; i++)");
		// prefix shift
		assertCorrect("for (int i = 0; i < 100; ++i)");
		assertCorrect("for (int i = 0; 100 == i; ++i)");
		
		// both shifts
		assertIncorrect("for (int i = 0; i < 100; ++i++)");
		// cmp without var
		assertIncorrect("for (int i = 0; 100 == 100; ++i++)");
	}
	
	private void assertCorrect(String input) {
		assertDoesNotThrow(() -> parse(input));
	}
	
	private void assertIncorrect(String input) {
		assertThrows(ParseException.class, () -> parse(input));
	}
	
	private Node parse(String input) throws ParseException {
		return new Parser().parse(input);
	}
	
}
