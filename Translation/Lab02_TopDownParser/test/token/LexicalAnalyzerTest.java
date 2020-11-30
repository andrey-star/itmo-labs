package token;

import org.junit.jupiter.api.Test;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static token.Token.*;

class LexicalAnalyzerTest {
	
	@Test
	public void test() throws ParseException {
		// sample
		testTokens("for (int x = 0; x < 10; x++)", List.of(
				FOR, OPEN_BRACKET,
				VAR, VAR, ASSIGN, NUMBER, SEMICOLON,
				VAR, LT, NUMBER, SEMICOLON,
				VAR, INC,
				CLOSE_BRACKET, END
		));
		// other tokens
		testTokens("for (var xxx = 100; xxx >= -100; xxx--)", List.of(
				FOR, OPEN_BRACKET,
				VAR, VAR, ASSIGN, NUMBER, SEMICOLON,
				VAR, GE, NUMBER, SEMICOLON,
				VAR, DEC,
				CLOSE_BRACKET, END
		));
		// random tokens
		testTokens("for for -- ++ x ;;() > < >= == <=-100", List.of(
				FOR, FOR, DEC, INC, VAR, SEMICOLON, SEMICOLON,
				OPEN_BRACKET, CLOSE_BRACKET, GT, LT, GE, EQ, LE, NUMBER, END
		));
		// numbers
		testTokens("0 -0 1 -100 100 1000000 4567", List.of(NUMBER, NUMBER, NUMBER, NUMBER, NUMBER, NUMBER, NUMBER, END));
		// empty
		testTokens("", List.of(END));
	}
	
	@Test
	private void testWhitespace() {
		assertCorrect("for (int x = 0; x < 10; x++)");
		assertCorrect("for(int x=0;x<10;x++)");
		assertCorrect("for   (int   x =  0;  x < 10; x++)");
		assertCorrect("for   (int   x =  0; \n x < 10; x++)");
		assertCorrect("for   (int   \tx =  0; \n\t x < 10; x++   )");
		assertCorrect("for   (   int   \tx =  0; \n\t x < 10; x++   )");
		assertCorrect("for   (   int   \tx =  0  ; \n\t x < 10  ;  x++   )");
	}
	
	private void assertCorrect(String input) {
		assertDoesNotThrow(() -> getTokens(input));
	}
	
	private void assertIncorrect(String input) {
		assertThrows(ParseException.class, () -> getTokens(input));
	}
	
	private void testTokens(String input, List<Token> expected) throws ParseException {
		List<Token> actual = getTokens(input);
		assertEquals(expected, actual);
	}
	
	
	private List<Token> getTokens(String input) throws ParseException {
		List<Token> tokens = new ArrayList<>();
		LexicalAnalyzer tokenizer = new LexicalAnalyzer(input);
		Token token;
		do {
			tokenizer.nextToken();
			token = tokenizer.curToken();
			tokens.add(token);
		} while (token != END);
		return tokens;
	}
	
}