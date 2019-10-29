package md2html;

public class Pair {
	int amount;
	int lastIndexOf;
	
	public Pair(int amount, int lastIndexOf) {
		this.amount = amount;
		this.lastIndexOf = lastIndexOf;
	}
	
	public int getLastIndexOf() {
		return lastIndexOf;
	}
}
