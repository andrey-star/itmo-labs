import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Scanner;

public class Task16_CombinationToNumber {
	
	private static BigInteger fact(int n) {
		BigInteger ans = BigInteger.ONE;
		for (int i = 2; i <= n; i++) {
			ans = ans.multiply(BigInteger.valueOf(i));
		}
		return ans;
	}
	
	private static long c(int n, int k) {
		return fact(n).divide(fact(n - k)).divide(fact(k)).longValue();
	}
	
	private static long gen(int[] a, int n, int k) {
		long numOfChoose = 0;
		for (int i = 0; i < k; i++) {
			int j = 1;
			if (i != 0) {
				j = a[i - 1] + 1;
			}
			for (; j <= a[i] - 1; j++) {
				numOfChoose += c(n - j, k - i - 1);
			}
		}
		return numOfChoose;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("choose2num.in"));
		int n = in.nextInt();
		int k = in.nextInt();
		int[] a = new int[k];
		for (int i = 0; i < a.length; i++) {
			a[i] = in.nextInt();
		}
		in.close();
		PrintWriter out = new PrintWriter(new File("choose2num.out"));
		out.println(gen(a, n, k));
		out.close();
	}
	
}