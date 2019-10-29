import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task08_AllCombinations {
	private static PrintWriter out;
	private static void gen(int[] a, int p, int n, int m) {
		if (p == m) {
			Arrays.stream(a).forEach(c -> out.print(c + " "));
			out.println();
			return;
		}
		int from = 1;
		if (p > 0) {
			from = a[p - 1] + 1;
		}
//		int to = m - (n - 1 - p);
		for (int i = from; i <= n; i++) {
			a[p] = i;
			gen(a, p + 1, n, m);
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		out = new PrintWriter(new File("choose.out"));
		Scanner in = new Scanner(new File("choose.in"));
		int n = in.nextInt();
		int m = in.nextInt();
		in.close();
		gen(new int[m], 0, n, m);
		out.close();
	}
	
}
