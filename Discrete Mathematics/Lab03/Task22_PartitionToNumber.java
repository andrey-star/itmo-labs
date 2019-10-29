import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

public class Task22_PartitionToNumber {
	
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
		for (long[] d : dp) {
			System.out.println(Arrays.toString(d));
		}
	}
	
	private static long gen(int n, String[] s) {
		int curMax = 0;
		int curSum = 0;
		int ans = 0;
		for (String number : s) {
			int el = Integer.parseInt(number);
			for (int j = curMax; j < el; j++) {
				System.out.println(n - curSum - j + " " + j);
				ans += dp[n - curSum - j][j];
			}
			curMax = el;
			curSum += el;
		}
		return ans;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("part2num.in"));
		String[] numbers = in.next().trim().split("\\+");
		int n = Arrays.stream(numbers).mapToInt(Integer::parseInt).sum();
		dp = new long[n + 1][n + 1];
		dp();
		PrintWriter out = new PrintWriter(new File("part2num.out"));
		out.println(gen(n, numbers));
		out.close();
	}
	
}