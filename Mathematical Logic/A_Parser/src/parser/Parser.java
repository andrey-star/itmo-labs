package parser;

import parser.exception.ParsingException;
import parser.expression.Expression;

public interface Parser {
	
	Expression parse(String expression) throws ParsingException;
	
}
