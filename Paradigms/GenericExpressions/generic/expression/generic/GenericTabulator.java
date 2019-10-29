package generic.expression.generic;

import generic.expression.TripleExpression;
import generic.expression.exceptions.EvaluatingException;
import generic.expression.exceptions.ParsingException;
import generic.expression.operation.type.*;
import generic.expression.parser.ExpressionParser;

import java.util.Map;

public class GenericTabulator implements Tabulator {
	
	private static final Map<String, Operation<?>> MODES = Map.of(
			"i", new CheckedIntegerOperation(),
			"d", new DoubleOperation(),
			"bi", new BigIntegerOperation(),
			"u", new IntegerOperation(),
			"f", new FloatOperation(),
			"b", new ByteOperation()
	);
	
	@Override
	public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
		if (x2 < x1 || y2 < y1 || z2 < z1) {
			throw new IllegalArgumentException("invalid bounds");
		}
		Object[][][] res = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
		Operation<?> operation = MODES.get(mode);
		if (operation == null) {
			return res;
		}
		return table(operation, expression, x1, x2, y1, y2, z1, z2, res);
	}
	
	private <T> Object[][][] table(Operation<T> operation, String exp, int x1, int x2, int y1, int y2, int z1, int z2, Object[][][] res) {
		ExpressionParser<T> parse = new ExpressionParser<>(operation);
		TripleExpression<T> expression;
		try {
			expression = parse.parse(exp);
		} catch (ParsingException e) {
			System.out.println(e.getMessage());
			return res;
		}
		for (int i = 0; i <= x2 - x1; i++) {
			for (int j = 0; j <= y2 - y1; j++) {
				for (int k = 0; k <= z2 - z1; k++) {
					try {
						res[i][j][k] = expression.evaluate(value(operation, x1 + i), value(operation, y1 + j), value(operation, z1 + k));
					} catch (EvaluatingException | ArithmeticException ignored) {
					}
				}
			}
		}
		return res;
	}
	
	private <T> T value(Operation<T> operation, int x) {
		return operation.parseNumber("" + x);
	}
}
