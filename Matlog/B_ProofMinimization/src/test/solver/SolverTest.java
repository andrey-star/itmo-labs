package test.solver;

import main.solver.Solver;
import main.solver.exception.IncorrectProofException;
import org.junit.BeforeClass;
import org.junit.Test;
import main.parser.MathLogicParser;
import main.parser.Parser;
import main.parser.exception.ParsingException;
import main.parser.grammar.Statement;
import main.parser.grammar.expression.Expression;
import main.solver.proof.ProvedExpression;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public class SolverTest {
	
	private static Parser parser;
	
	@BeforeClass
	public static void init() {
		parser = new MathLogicParser();
	}
	
	@Test
	public void test_checkHypothesis() throws ParsingException, IncorrectProofException {
		checkHypothesis("A -> B", true, "A->B", "!B");
		checkHypothesis("!B", true, "A->B", "!B");
		checkHypothesis("A -> B     ->C", true, "A->B", "!B", "A -> B-> C");
		checkHypothesis("!B", false, "A->B");
		checkHypothesis("B  ->C", false, "A->B", "A -> B -> C");
	}
	
	@Test
	public void test_solve() throws ParsingException {
		test_solve(
				"|- A -> A",
				"A & A -> A",
				"A -> A -> A",
				"A -> (A -> A) -> A",
				"A & A -> A",
				"(A -> A -> A) -> (A -> (A -> A) -> A) -> A -> A",
				"(A -> (A -> A) -> A) -> A -> A",
				"A & A -> A",
				"A -> A"
		);
		test_solve(
				"A->B, !B |- !A",
				"A->B",
				"!B",
				"!B -> A -> !B",
				"A -> !B",
				"(A -> B) -> (A -> !B) -> !A",
				"(A -> !B) -> !A",
				"!A");
		test_solve(
				"A, C |- B’",
				"B’");
	}
	
	private void test_solve_file() throws IOException, ParsingException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		List<String> lines = in.lines().collect(Collectors.toList());
		in.close();
		System.out.println();
		System.out.println(lines.stream().map(s -> "\"" + s + "\"").collect(Collectors.joining(",\n")));
		System.out.println();
		test_solve(lines);
	}
	
	private void test_solve(String... linesArr) throws ParsingException {
		test_solve(Arrays.asList(linesArr));
	}
	
	private void test_solve(List<String> lines) throws ParsingException {
		Statement statement = parser.parseStatement(lines.get(0));
		List<Expression> expressions = new ArrayList<>();
		for (String line : lines.subList(1, lines.size())) {
			expressions.add(parser.parseExpression(line));
		}
		Solver solver = new Solver(statement, expressions, Solver.getAxioms(parser));
		try {
			System.out.println(solver.solve());
		} catch (IncorrectProofException e) {
			e.printStackTrace();
		}
		System.out.println();
	}
	
	
	@Test
	public void test_axiom() throws ParsingException, IncorrectProofException {
		List<String> axioms = Solver.axiomsStr;
		Random random = new Random();
		for (int i = 0; i < 10; i++) {
			String axiom = axioms.get(random.nextInt(axioms.size()));
			String a = "((X->Y)-> (X->Z) -> (Z->Y))";
			String b = "((X&Y&Z) | (X|Z) -> (!X|!Z))";
			String c = "((X&!Z) -> !Z -> (!X&!Y -> !Z))";
			String replace = axiom.replace("A", a).replace("B", b).replace("C", c);
			checkAxiom(replace, axiom);
			replace = axiom.replace("A", a).replace("B", a).replace("C", a);
			checkAxiom(replace, axiom);
		}
		checkAxiom("A -> B -> C");
		checkAxiom("A|B->A");
		checkAxiom("A->A");
		checkAxiom("A");
		checkAxiom("A->!!A");
	}
	
	private void checkAxiom(String expression) throws ParsingException {
		Solver solver = new Solver(parser.parseStatement("|-"), null, Solver.getAxioms(parser));
		try {
			solver.checkAxiom(parser.parseExpression(expression));
			fail(expression + " is not an axiom");
		} catch (IncorrectProofException ignored) {
		} catch (ParsingException e) {
			System.err.println("Error while parsing expression: " + expression);
			throw e;
		}
	}
	
	private void checkAxiom(String expression, String axiom) throws ParsingException, IncorrectProofException {
		Solver solver = new Solver(parser.parseStatement("|-"), null, Solver.getAxioms(parser));
		try {
			ProvedExpression actual = solver.checkAxiom(parser.parseExpression(expression));
			assertNotEquals(expression + " is axiom " + axiom, null, actual);
		} catch (ParsingException e) {
			System.err.println("Error while parsing expression: " + expression);
			throw e;
		} catch (IncorrectProofException e) {
			System.err.println(expression + " is axiom " + axiom);
			throw e;
		}
	}
	
	private void checkHypothesis(String expression, boolean isCorrect, String... hyps) throws ParsingException, IncorrectProofException {
		String statement = String.join(",", hyps) + "|-";
		try {
			Solver solver = new Solver(parser.parseStatement(statement), null, null);
			try {
				solver.checkHypothesis(parser.parseExpression(expression));
				if (!isCorrect) {
					fail(expression + "expression is not hypothesis from: " + Arrays.toString(hyps));
				}
			} catch (ParsingException e) {
				System.err.println("Error while parsing expression: " + expression);
				throw e;
			} catch (IncorrectProofException e) {
				if (isCorrect) {
					System.err.println(expression + "expression is a hypothesis from: " + Arrays.toString(hyps));
					throw e;
				}
			}
		} catch (ParsingException e) {
			System.err.println("Error while parsing statements: " + Arrays.toString(hyps));
			throw e;
		}
	}
	
}