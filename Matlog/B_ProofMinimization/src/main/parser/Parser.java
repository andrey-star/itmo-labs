package main.parser;

import main.parser.exception.ParsingException;
import main.parser.grammar.Statement;
import main.parser.grammar.expression.Expression;

public interface Parser {
	
	Expression parseExpression(String expression) throws ParsingException;
	
	Statement parseStatement(String statement) throws ParsingException;
	
}
