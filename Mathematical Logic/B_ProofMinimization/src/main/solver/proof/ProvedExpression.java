package main.solver.proof;

import main.parser.grammar.expression.Expression;

public class ProvedExpression {
	
	private final Expression expression;
	private final Proof proof;
	private int index;
	
	public ProvedExpression(Expression expression, Proof proof, int index) {
		this.expression = expression;
		this.proof = proof;
		this.index = index;
	}
	
	public Expression getExpression() {
		return expression;
	}
	
	public Proof getProof() {
		return proof;
	}
	
	public int getIndex() {
		return index;
	}
	
	public void setIndex(int index) {
		this.index = index;
	}
	
	@Override
	public String toString() {
		return String.format("[%s. %s] %s", index + 1, proof, expression);
	}
}
