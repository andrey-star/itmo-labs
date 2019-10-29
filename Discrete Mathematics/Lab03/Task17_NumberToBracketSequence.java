import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Scanner;

public class Task17_NumberToBracketSequence {
	
	private static long[][] dp;
	
	private static void dp() {
		dp[0][0] = 1;
		for (int i = 1; i < dp.length; i++) {
			for (int j = 0; j < dp[0].length; j++) {
				long ifOpen = j - 1 >= 0 ? dp[i - 1][j - 1] : 0;
				long ifClosed = j + 1 < dp[0].length ? dp[i - 1][j + 1] : 0;
				dp[i][j] = ifOpen + ifClosed;
			}
		}
	}
	
	private static String gen(int n, long k) {
		StringBuilder seq = new StringBuilder();
		int diff = 0;
		for (int i = 0; i < 2 * n; i++) {
			if (dp[2 * n - i - 1][diff + 1] > k) {
				seq.append("(");
				diff++;
			} else {
				long skip = dp[2 * n - i - 1][diff + 1];
				k -= skip;
				seq.append(")");
				diff--;
			}
		}
		return seq.toString();
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("num2brackets.in"));
		int n = in.nextInt();
		long k = in.nextLong();
		in.close();
		dp = new long[2 * n + 1][n + 2];
		dp();
		PrintWriter out = new PrintWriter(new File("num2brackets.out"));
		out.println(gen(n, k));
		out.close();
	}
	
}