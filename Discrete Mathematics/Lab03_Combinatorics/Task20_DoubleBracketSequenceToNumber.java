import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class Task20_DoubleBracketSequenceToNumber {
	
	private static final long[][] dp;
	
	static {
		dp = new long[50][50];
		dp[0][0] = 1;
		for (int i = 1; i < dp.length; i++) {
			for (int j = 0; j < dp[0].length; j++) {
				long ifOpen = j - 1 >= 0 ? dp[i - 1][j - 1] : 0;
				long ifClosed = j + 1 < dp[0].length ? dp[i - 1][j + 1] : 0;
				dp[i][j] = ifOpen + ifClosed;
			}
		}
	}
	
	private static long gen(int n, String seq) {
		int diff = 0;
		long ans = 0;
		for (int i = 0; i < 2 * n; i++) {
			if (seq.charAt(i) == '(' || seq.charAt(i) == '[') {
				diff++;
			} else {
				ans += dp[2 * n - i - 1][diff + 1] * (1L << ((2 * n - i - 1 - diff) / 2));
				diff--;
			}
		}
		return ans;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("brackets2num2.in"));
		String seq = in.next();
		int n = seq.length() / 2;
		in.close();
		PrintWriter out = new PrintWriter(new File("brackets2num2.out"));
		System.out.println(gen(n, seq));
		out.close();
	}
	
}