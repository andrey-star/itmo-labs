import java.io.*;

public class E_Levenshtein {

	private static int getDistance(String s1, String s2) {
		int n = s1.length();
		int m = s2.length();
		int[][] dp = new int[n + 1][m + 1];

		for (int i = 0; i < n + 1; i++) {
			for (int j = 0; j < m + 1; j++) {
				if (i == 0 && j == 0) {
					dp[i][j] = 0;
				} else if (i == 0) {
					dp[i][j] = j;
				} else if (j == 0) {
					dp[i][j] = i;
				} else {
					if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
						dp[i][j] = dp[i - 1][j - 1];
					} else {
						dp[i][j] = Math.min(dp[i][j - 1] + 1, dp[i - 1][j] + 1);
						dp[i][j] = Math.min(dp[i][j], dp[i - 1][j - 1] + 1);
					}
				}
			}
		}
		return dp[n][m];
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("levenshtein.in")));
		String s1 = in.readLine();
		String s2 = in.readLine();
		PrintWriter out = new PrintWriter(new File("levenshtein.out"));
		out.println(getDistance(s1, s2));
		out.close();
	}

}