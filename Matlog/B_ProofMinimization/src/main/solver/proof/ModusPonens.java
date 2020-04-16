package main.solver.proof;

public class ModusPonens implements Proof {
	
	private int big;
	private int small;
	
	public ModusPonens(int big, int small) {
		this.big = big;
		this.small = small;
	}
	
	@Override
	public String toString() {
		return "M.P. " + (big + 1) + ", " + (small + 1);
	}
	
	public int getBig() {
		return big;
	}
	
	public void setBig(int big) {
		this.big = big;
	}
	
	public int getSmall() {
		return small;
	}
	
	public void setSmall(int small) {
		this.small = small;
	}
}
