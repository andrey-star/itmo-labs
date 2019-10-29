import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Scanner;

public class Task15_NumberToCombination {
	
	private static BigInteger fact(int n) {
		BigInteger ans = BigInteger.ONE;
		for (int i = 2; i <= n; i++) {
			ans = ans.multiply(BigInteger.valueOf(i));
		}
		return ans;
	}
	
	private static BigInteger c(int n, int k) {
		return fact(n).divide(fact(n - k)).divide(fact(k));
	}
	
	private static ArrayList<Integer> gen(ArrayList<Integer> a, int n, int k, long t) {
		BigInteger m = BigInteger.valueOf(t);
		int cur = 1;
		while (k > 0) {
			if (m.compareTo(c(n - 1, k - 1)) >= 0) {
				m = m.subtract(c(n - 1, k - 1));
			} else {
				a.add(cur);
				k--;
			}
			cur++;
			n--;
		}
		return a;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("num2choose.in"));
		int n = in.nextInt();
		int k = in.nextInt();
		long m = in.nextLong();
		in.close();
		PrintWriter out = new PrintWriter(new File("num2choose.out"));
		gen(new ArrayList<>(), n, k, m).forEach(i -> out.print(i + " "));
		out.close();
	}
	
}