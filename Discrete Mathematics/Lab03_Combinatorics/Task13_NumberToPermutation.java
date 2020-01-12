import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task13_NumberToPermutation {
	
	private static boolean contains(int[] a, int x) {
		for (int i : a) {
			if (i == x) {
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
	
	private static void gen(int[] a, int n, long k, int p) {
		if (p == n) {
			return;
		}
		long skip = k / fact(n - p - 1);
		int index = 0;
		for (int j = 0; j < n; j++) {
			if (!contains(a, j)) {
				index++;
				if (index == skip + 1) {
					a[p] = j;
				}
			}
		}
		gen(a, n, k % fact(n - p - 1), p + 1);
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("num2perm.in"));
		int n = in.nextInt();
		long k = in.nextLong();
		in.close();
		int[] a = new int[n];
		Arrays.fill(a, -1);
		gen(a, n, k, 0);
		PrintWriter out = new PrintWriter(new File("num2perm.out"));
		Arrays.stream(a).forEach(i -> out.print(i + 1 + " "));
		out.close();
	}
	
}