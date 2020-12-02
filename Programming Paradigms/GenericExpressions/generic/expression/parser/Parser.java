package generic.expression.parser;

import generic.expression.TripleExpression;
import generic.expression.exceptions.ParsingException;

public interface Parser<T> {
    TripleExpression<T> parse(String expressionn) throws ParsingException;
}