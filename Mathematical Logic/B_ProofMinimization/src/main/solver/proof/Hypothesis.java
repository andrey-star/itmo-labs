package main.solver.proof;

public class Hypothesis implements Proof {
	
	private int index;
	
	public Hypothesis(int index) {
		this.index = index;
	}
	
	public int getIndex() {
		return index;
	}
	
	public void setIndex(int index) {
		this.index = index;
	}
	
	@Override
	public String toString() {
		return "Hypothesis " + (index + 1);
	}
}
