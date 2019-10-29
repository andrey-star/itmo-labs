package generic.expression;

import generic.expression.exceptions.EvaluatingException;
import generic.expression.exceptions.ParsingException;
import generic.expression.operation.type.CheckedIntegerOperation;
import generic.expression.parser.ExpressionParser;

public class Main {
	
	public static void main(String[] args) {
		String expression = "(-1376383951) * (-128004972)";
		ExpressionParser<Integer> parser = new ExpressionParser<>(new CheckedIntegerOperation());
		System.out.println(expression);
		try {
			TripleExpression<Integer> exp = parser.parse(expression);
			int x = 1;
			int y = 1;
			int z = 2;
			System.out.println("Res: " + exp.evaluate(x, y, z));
		} catch (EvaluatingException | ParsingException e) {
			System.out.println(e.getMessage());
		}
	}
	
}
