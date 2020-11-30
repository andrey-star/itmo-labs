import java.util.List;

public class Entry {
	List<Integer> x;
	int y;
	String filename;
	
	public Entry(List<Integer> x, int y, String filename) {
		this.x = x;
		this.y = y;
		this.filename = filename;
	}
	
	@Override
	public String toString() {
		return filename + ": " + x + " -> " + y;
	}
}
