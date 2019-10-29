import java.io.*;

public class F_Templates {
	
	private static boolean isMatchable(String s1, String s2) {
		int n = s1.length();
		int m = s2.length();
		boolean[][] dp = new boolean[n + 1][m + 1];
		dp[0][0] = true;
		for (int i = 1; i < n + 1; i++) {
			if (s1.charAt(i - 1) != '*') {
				break;
			}
			dp[i][0] = true;
		}
		for (int i = 1; i < n + 1; i++) {
			for (int j = 1; j < m + 1; j++) {
				if (s1.charAt(i - 1) == '*') {
					dp[i][j] = dp[i - 1][j] || dp[i][j - 1];
				} else if (s1.charAt(i - 1) == '?') {
					dp[i][j] = dp[i - 1][j - 1];
				} else {
					if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
						dp[i][j] = dp[i - 1][j - 1];
					} else {
						dp[i][j] = false;
					}
				}
			}
		}
		return dp[n][m];
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s1 = in.readLine();
		String s2 = in.readLine();
		System.out.println(isMatchable(s1, s2) ? "YES" : "NO");
	}
	
}