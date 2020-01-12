import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task21_NumberToPartition {
	
	private static long[][] dp;
	
	private static void dp() {
		for (int i = 1; i < dp.length; i++) {
			for (int j = dp[0].length - 1; j > 0; j--) {
				if (i == j) {
					dp[i][j] = 1;
				} else if (i < j) {
					dp[i][j] = 0;
				} else {
					dp[i][j] = dp[i][j + 1] + dp[i - j][j];
				}
			}
		}
	}
	
	private static String gen(int n, int k) {
		StringBuilder res = new StringBuilder();
		int curSum = 0;
		int curMax = 0;
		while (curSum != n) {
			for (int j = curMax + 1; j <= n; j++) {
				System.out.println(curSum + " " + j);
				if (dp[n - curSum - j][j] <= k) {
					res.append(j).append("+");
					k -= dp[n - curSum - j][j];
					curSum += j;
					curMax = j;
					break;
				}
			}
		}
		return res.toString();
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("num2part.in"));
		int n = in.nextInt();
		int k = in.nextInt();
		dp = new long[n + 1][n + 1];
		dp();
		PrintWriter out = new PrintWriter(new File("num2part.out"));
		System.out.println(gen(n, k));
		out.close();
	}
	
}