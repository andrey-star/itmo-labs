package main;

import main.parser.exception.ParsingException;
import main.parser.grammar.Statement;
import main.parser.grammar.expression.Expression;
import main.parser.MathLogicParser;
import main.parser.Parser;
import main.solver.Solver;
import main.solver.exception.IncorrectProofException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Main {
	
	public static void main(String[] args) throws IOException, ParsingException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		List<String> lines = in.lines().collect(Collectors.toList());
		in.close();
		
		Parser parser = new MathLogicParser();
		Statement statement = parser.parseStatement(lines.get(0));
		List<Expression> expressions = new ArrayList<>();
		for (String line : lines.subList(1, lines.size())) {
			expressions.add(parser.parseExpression(line));
		}
		Solver solver = new Solver(statement, expressions, Solver.getAxioms(parser));
		try {
			System.out.println(solver.solve());
		} catch (IncorrectProofException e) {
			System.out.println(e.getMessage());
		}
	}
	
}
