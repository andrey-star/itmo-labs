package expression;

import expression.exceptions.EvaluatingException;
import expression.exceptions.ParsingException;
import expression.parser.*;

public class Main {
	
	public static void main(String[] args) {
		String expression = "((5)";
		ExpressionParser parser = new ExpressionParser();
		System.out.println(expression);
		try {
			TripleExpression exp = parser.parse(expression);
			int x = 1;
			int y = 1;
			int z = 2;
			System.out.println("Res: " + exp.evaluate(x, y, z));
		} catch (EvaluatingException | ParsingException e) {
			System.out.println(e.getMessage());
		}
	}
	
}
