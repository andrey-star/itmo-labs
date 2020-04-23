package test.parser;

import main.parser.MathLogicParser;
import org.junit.Assert;
import org.junit.Test;
import main.parser.exception.ParsingException;
import main.parser.grammar.Statement;
import main.parser.grammar.expression.Expression;

public class ParserTest {
	
	@Test
	public void testParseStatement() throws ParsingException {
		testParseStatement("|- A -> A", "|-(->,A,A)");
		testParseStatement("A->B, !B |- !A", "(->,A,B),(!B)|-(!A)");
		testParseStatement("(A -> B), !B |- !A", "(->,A,B),(!B)|-(!A)");
		testParseStatement("(    A ->     B), !    B    |-    !  A",
				"(->,A,B),(!B)|-(!A)");
		testParseStatement("A, C |- B'", "A,C|-B'");
	}
	
	@Test
	public void testParseExpression() throws ParsingException {
		testParseExpression("A",
				"A");
		testParseExpression("!A&!B->!(A|B)",
				"(->,(&,(!A),(!B)),(!(|,A,B)))");
		testParseExpression("!  A & !      B   ->   !(  A  |   B   )",
				"(->,(&,(!A),(!B)),(!(|,A,B)))");
		testParseExpression("P1’->!QQ->!R10&S|!T&U&V",
				"(->,P1’,(->,(!QQ),(|,(&,(!R10),S),(&,(&,(!T),U),V))))");
	}
	
	private void testParseExpression(String input, String expected) throws ParsingException {
		try {
			Assert.assertEquals(input, expected, parseExpression(input).toString());
		} catch (ParsingException e) {
			System.err.format("Error while parsing '%s'%n", input);
			throw e;
		}
	}
	
	private Expression parseExpression(String expression) throws ParsingException {
		return new MathLogicParser().parseExpression(expression);
	}
	
	private void testParseStatement(String input, String expected) throws ParsingException {
		try {
			Assert.assertEquals(input, expected, parseStatement(input).toString());
		} catch (ParsingException e) {
			System.err.format("Error while parsing '%s'%n", input);
			throw e;
		}
	}
	
	private Statement parseStatement(String statement) throws ParsingException {
		return new MathLogicParser().parseStatement(statement);
	}
}