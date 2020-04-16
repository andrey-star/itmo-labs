package main.solver.proof;

public class Axiom implements Proof {
	
	private int index;
	
	public Axiom(int index) {
		this.index = index;
	}
	
	public void setIndex(int index) {
		this.index = index;
	}
	
	@Override
	public String toString() {
		return "Ax. sch. " + (index + 1);
	}
}
