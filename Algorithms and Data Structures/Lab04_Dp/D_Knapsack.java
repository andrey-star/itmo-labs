import java.io.*;
import java.util.ArrayList;
import java.util.Collections;

public class D_Knapsack {
	
	private static void dp(int n, int k, int[] m, int[] c, int[][] dp) {
		for (int i = 0; i < n + 1; i++) {
			dp[i][0] = 0;
		}
		for (int i = 0; i < k + 1; i++) {
			dp[0][i] = 0;
		}
		for (int i = 1; i < n + 1; i++) { // indices possible to use
			for (int j = 1; j < k + 1; j++) { // knapsack size
				dp[i][j] = dp[i - 1][j];
				if (j >= m[i - 1]) {
					dp[i][j] = Math.max(dp[i][j], dp[i - 1][j - m[i - 1]] + c[i - 1]);
				}
			}
		}
	}
	
	private static ArrayList<Integer> a;
	
	private static void rev(int[][] dp, int[] m, int n, int k) {
		while (dp[n][k] != 0) {
			if (dp[n][k] == dp[n - 1][k]) {
				n--;
			} else {
				a.add(n);
				k -= m[n - 1];
				n--;
			}
		}
		Collections.reverse(a);
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("knapsack.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		int[] m = new int[n];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			m[i] = Integer.parseInt(line[i]);
		}
		int[] c = new int[n];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			c[i] = Integer.parseInt(line[i]);
		}
		in.close();
		int[][] dp = new int[n + 1][k + 1]; // [limit][capacity]
		dp(n, k, m, c, dp);
		a = new ArrayList<>();
		rev(dp, m, n, k);
		PrintWriter out = new PrintWriter(new File("knapsack.out"));
		out.println(a.size());
		for (int i : a) {
			out.print(i + " ");
		}
		out.close();
	}
	
}
