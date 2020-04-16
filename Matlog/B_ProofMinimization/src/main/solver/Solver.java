package main.solver;

import main.parser.Parser;
import main.parser.exception.ParsingException;
import main.parser.grammar.Statement;
import main.parser.grammar.expression.*;
import main.solver.exception.IncorrectProofException;
import main.solver.proof.*;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Solver {
	
	public static final List<String> axiomsStr;
	
	static {
		axiomsStr = Stream.of(
				"A->B->A",
				"(A->B)->(A->B->C)->(A->C)",
				"A->B->A&B",
				"A&B->A",
				"A&B->B",
				"A->A|B",
				"B->A|B",
				"(A->C)->(B->C)->(A|B->C)",
				"(A->B)->(A->!B)->!A",
				"!!A->A")
		                  .collect(Collectors.toList());
	}
	
	private final Expression statement;
	private final List<Expression> hypothesis;
	private final List<Expression> proof;
	private final List<Expression> axioms;
	private final Map<Expression, Integer> expr2ind = new HashMap<>();
	private final Map<Integer, Expression> ind2expr = new HashMap<>();
	private final Map<Expression, List<Integer>> right2expr = new HashMap<>();
	
	private int index = 0;
	
	public Solver(Statement statement, List<Expression> proof, List<Expression> axioms) {
		this.hypothesis = statement.getHypothesis();
		this.statement = statement.getExpression();
		this.proof = proof;
		this.axioms = axioms;
	}
	
	public static List<Expression> getAxioms(Parser parser) throws ParsingException {
		List<Expression> axioms = new ArrayList<>();
		for (String ax : axiomsStr) {
			axioms.add(parser.parseExpression(ax));
		}
		return axioms;
	}
	
	public String solve() throws IncorrectProofException {
		if (proof.size() == 0 || !proof.get(proof.size() - 1).equals(statement)) {
			throw new IncorrectProofException();
		}
		List<ProvedExpression> provedExpressions = new ArrayList<>();
		int indexOfStatement = proveAll(provedExpressions);
		if (indexOfStatement == -1) {
			throw new IncorrectProofException();
		}
		return minimize(provedExpressions, indexOfStatement);
	}
	
	private String minimize(List<ProvedExpression> provedExpressions, int indexOfStatement) {
		Set<Integer> used = new TreeSet<>();
		traverse(indexOfStatement, provedExpressions, used);
		List<Integer> indices = new ArrayList<>(used);
		List<ProvedExpression> necessary = indices.stream().map(provedExpressions::get).collect(Collectors.toList());
		Map<Integer, Integer> newIndex = new HashMap<>();
		for (int i = 0; i < necessary.size(); i++) {
			newIndex.put(indices.get(i), i);
		}
		for (ProvedExpression provedExpression : necessary) {
			provedExpression.setIndex(newIndex.get(provedExpression.getIndex()));
			Proof proof = provedExpression.getProof();
			if (proof instanceof ModusPonens) {
				ModusPonens mp = (ModusPonens) proof;
				mp.setBig(newIndex.get(mp.getBig()));
				mp.setSmall(newIndex.get(mp.getSmall()));
			}
		}
		String hyp = hypothesis.stream()
		                       .map(Expression::toString)
		                       .collect(Collectors.joining(", "));
		StringBuilder sb = new StringBuilder(hyp);
		if (sb.length() != 0) {
			sb.append(" ");
		}
		sb.append("|- ").append(statement.toString());
		for (ProvedExpression provedExpression : necessary) {
			sb.append("\n");
			sb.append(provedExpression);
		}
		return sb.toString();
	}
	
	private void traverse(int index, List<ProvedExpression> provedExpressions, Set<Integer> used) {
		if (used.contains(index)) {
			return;
		}
		used.add(index);
		Proof proof = provedExpressions.get(index).getProof();
		if (proof instanceof ModusPonens) {
			ModusPonens mp = (ModusPonens) proof;
			traverse(mp.getBig(), provedExpressions, used);
			traverse(mp.getSmall(), provedExpressions, used);
		}
	}
	
	public int proveAll(List<ProvedExpression> pes) throws IncorrectProofException {
		int indexOfStatement = -1;
		for (Expression expression : proof) {
			if (expr2ind.containsKey(expression)) {
				continue;
			}
			ProvedExpression pe = checkExpression(expression);
			if (indexOfStatement == -1) {
				pes.add(pe);
			}
			if (expression.equals(statement)) {
				indexOfStatement = index;
			}
			expr2ind.put(expression, index);
			ind2expr.put(index, expression);
			if (expression instanceof Impl) {
				Expression right = ((BinaryExpression) expression).getSecond();
				right2expr.computeIfAbsent(right, k -> new ArrayList<>());
				right2expr.get(right).add(index);
			}
			index++;
		}
		return indexOfStatement;
	}
	
	private ProvedExpression checkExpression(Expression expression) throws IncorrectProofException {
		try {
			return checkHypothesis(expression);
		} catch (IncorrectProofException e) {
			try {
				return checkAxiom(expression);
			} catch (IncorrectProofException e1) {
				return checkModusPonens(expression);
			}
		}
	}
	
	public ProvedExpression checkModusPonens(Expression expression) throws IncorrectProofException {
		List<Integer> candidates = right2expr.get(expression);
		if (candidates == null) {
			throw new IncorrectProofException();
		}
		for (int bigIndex : candidates) {
			Expression big = ind2expr.get(bigIndex);
			Integer smallIndex = expr2ind.get(((BinaryExpression) big).getFirst());
			if (smallIndex == null) {
				continue;
			}
			return new ProvedExpression(expression, new ModusPonens(bigIndex, smallIndex), index);
		}
		throw new IncorrectProofException();
	}
	
	public ProvedExpression checkAxiom(Expression expression) throws IncorrectProofException {
		for (int i = 0; i < axioms.size(); i++) {
			Expression axiom = axioms.get(i);
			if (matches(axiom, expression, new HashMap<>())) {
				return new ProvedExpression(expression, new Axiom(i), index);
			}
		}
		throw new IncorrectProofException();
	}
	
	private boolean matches(Expression axiom, Expression expression, Map<Expression, Expression> varToExpr) {
		if (axiom instanceof Variable) {
			Expression varExpr = varToExpr.computeIfAbsent(axiom, ax -> expression);
			return varExpr.equals(expression);
		}
		if (axiom.getClass() != expression.getClass()) {
			return false;
		}
		if (axiom instanceof BinaryExpression) {
			return axiomEquals((BinaryExpression) axiom, ((BinaryExpression) expression), varToExpr);
		}
		return axiomEquals((UnaryExpression) axiom, (UnaryExpression) expression, varToExpr);
	}
	
	private boolean axiomEquals(BinaryExpression axiom, BinaryExpression expression, Map<Expression, Expression> varToExpr) {
		return axiom.getOperator().equals(expression.getOperator())
				&& matches(axiom.getFirst(), expression.getFirst(), varToExpr)
				&& matches(axiom.getSecond(), expression.getSecond(), varToExpr);
	}
	
	private boolean axiomEquals(UnaryExpression axiom, UnaryExpression expression, Map<Expression, Expression> varToExpr) {
		return axiom.getOperator().equals(expression.getOperator())
				&& matches(axiom.getExpression(), expression.getExpression(), varToExpr);
	}
	
	public ProvedExpression checkHypothesis(Expression expression) throws IncorrectProofException {
		for (int i = 0; i < hypothesis.size(); i++) {
			Expression hyp = hypothesis.get(i);
			if (hyp.equals(expression)) {
				return new ProvedExpression(expression, new Hypothesis(i), index);
			}
		}
		throw new IncorrectProofException();
	}
	
}
