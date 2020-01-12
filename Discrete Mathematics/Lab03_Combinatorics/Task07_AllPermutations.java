import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class Task07_AllPermutations {
	
	private static PrintWriter out;
	
	private static boolean contains(int[] a, int x, int bound) {
		for (int i = 0; i < bound; i++) {
			int f = a[i];
			if (f == x) {
				return true;
			}
		}
		return false;
	}
	
	private static void gen(int[] p, int n, int length) {
		if (length == n) {
			Arrays.stream(p).forEach(i -> out.print(i + " "));
			out.println();
			return;
		}
		
		for (int i = 1; i <= n; i++) {
			if (!contains(p, i, length)) {
				p[length] = i;
				gen(p, n, length + 1);
			}
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		out = new PrintWriter(new File("permutations.out"));
//		Scanner in = new Scanner(new File("permutations.in"));
//		int n = in.nextInt();
//		in.close();
		int n = 5;
		gen(new int[n], n, 0);
		out.close();
	}
	
}
