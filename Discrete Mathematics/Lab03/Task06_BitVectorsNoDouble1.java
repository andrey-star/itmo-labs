import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

public class Task06_BitVectorsNoDouble1 {
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("vectors.in"));
		int n = in.nextInt();
		in.close();
		ArrayList<String> vectors = new ArrayList<>();
		for (int i = 0; i < (1 << n); i++) {
			StringBuilder vector = new StringBuilder();
			for (int j = 0; j < n; j++) {
				vector.append((i >> (n - j - 1)) & 1);
			}
			boolean okay = true;
			for (int j = 1; j < n; j++) {
				if (vector.charAt(j) == vector.charAt(j - 1) && vector.charAt(j) == '1') {
					okay = false;
					break;
				}
			}
			if (okay) {
				vectors.add(vector.toString());
			}
		}
		PrintWriter out = new PrintWriter(new File("vectors.out"));
		out.println(vectors.size());
		for (String vector : vectors) {
			out.println(vector);
		}
		out.close();
		
	}
	
}
