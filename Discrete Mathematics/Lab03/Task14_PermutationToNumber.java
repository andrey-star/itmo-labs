import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;
import java.util.TreeSet;

public class Task14_PermutationToNumber {
	
	private static boolean contains(int[] a, int x, int bound) {
		for (int i = 0; i < bound; i++) {
			if (a[i] == x) {
				return true;
			}
		}
		return false;
	}
	
	private static long fact(int n) {
		if (n == 0) {
			return 1;
		}
		return n * fact(n - 1);
	}
	
	private static int minPossible(int[] a, int n, int pos) {
		int minPossible = n + 1;
		for (int i = 0; i < n; i++) {
			if (!contains(a, i + 1, pos)) {
				if (minPossible > i + 1) {
					minPossible = i + 1;
				}
			}
		}
		return minPossible;
	}
	
	private static int skipped(int[] a, int n, int pos, int x) {
		int minPossible = minPossible(a, n, pos);
		int skipped = 0;
		for (int i = minPossible; i < x; i++) {
			if (!contains(a, i, pos)) {
				skipped++;
			}
		}
		return skipped;
	}
	
	private static long gen(int[] a, int n) {
		long res = 0;
		for (int i = 0; i < n; i++) {
			res += skipped(a, n, i, a[i]) * fact(n - i - 1);
		}
		return res;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("perm2num.in"));
		int n = in.nextInt();
		int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = in.nextInt();
		}
		in.close();
		long k = gen(a, n);
		PrintWriter out = new PrintWriter(new File("perm2num.out"));
		out.println(k);
		out.close();
	}
	
}