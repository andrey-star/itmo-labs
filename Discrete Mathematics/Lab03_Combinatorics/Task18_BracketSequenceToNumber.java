import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task18_BracketSequenceToNumber {
	
	private static long[][] dp;
	
	private static void dp(int n) {
		dp = new long[2 * n + 1][2 * n + 1];
		dp[0][0] = 1;
		for (int i = 1; i < dp.length; i++) {
			for (int j = 0; j < dp[0].length; j++) {
				long ifOpen = j - 1 >= 0 ? dp[i - 1][j - 1] : 0;
				long ifClosed = j + 1 < dp[0].length ? dp[i - 1][j + 1] : 0;
				dp[i][j] = ifOpen + ifClosed;
			}
		}
		for (long[] d : dp) {
			System.out.println(Arrays.toString(d));
		}
	}
	
	private static long gen(int n, String seq) {
		int diff = 0;
		long ans = 0;
		for (int i = 0; i < 2 * n; i++) {
			if (seq.charAt(i) == '(') {
				diff++;
			} else {
				System.out.println(2 * n - i - 1 + " " + (diff + 1));
				ans += dp[2 * n - i - 1][diff + 1];
				diff--;
			}
		}
		return ans;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("brackets2num.in"));
		String seq = in.next();
		in.close();
		int n = seq.length() / 2;
		dp(n);
		PrintWriter out = new PrintWriter(new File("brackets2num.out"));
		out.println(gen(n, seq));
		out.close();
	}
	
}