import java.io.*;
import java.util.Arrays;

public class K_Subpalyndrom {
	
	private static final int MOD = (int) 1e9;
	
	private static int dp(int[][] dp, int[] a, int l, int r) {
		if (l > r) {
			return 0;
		}
		if (dp[l][r] != -1) {
			return dp[l][r];
		}
		int ans = ((dp(dp, a, l + 1, r) + dp(dp, a, l, r - 1)) % MOD - dp(dp, a, l + 1, r - 1) + MOD) % MOD;
		if (a[l] == a[r]) {
			ans = (ans + 1 + dp(dp, a, l + 1, r - 1)) % MOD;
		}
		dp[l][r] = ans;
		return ans;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		String[] line = in.readLine().trim().split(" +");
		int[] a  = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		int[][] dp = new int[n][n];
		for (int[] d : dp) {
			Arrays.fill(d, -1);
		}
		PrintWriter out = new PrintWriter(System.out);
		out.println(dp(dp, a, 0, n - 1));
		out.close();
		
	}
	
}