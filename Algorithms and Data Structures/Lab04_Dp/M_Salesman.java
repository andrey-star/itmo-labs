import java.io.*;
import java.util.Arrays;

public class M_Salesman {
	
	private static void dp(long[][] dp, int[][] g, int n) {
		for (int i = 0; i < n; i++) {
			dp[1 << i][i] = 0;
		}
		for (int mask = 1; mask < 1 << n; mask++) {
			for (int i = 0; i < n; i++) { // last
				if (((mask >> i) & 1) == 1) { // if i in mask
					for (int j = 0; j < n; j++) { // all neighbours of last
						if (((mask >> j) & 1) == 1 && g[i][j] != Integer.MAX_VALUE) { // if j in mask and i <-> j
							if (dp[mask - (1 << i)][j] != Long.MAX_VALUE) { // if j is reachable
								dp[mask][i] = Math.min(dp[mask][i], dp[mask - (1 << i)][j] + g[i][j]);
							}
						}
					}
				}
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("salesman.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int[][] g = new int[n][n];
		for (int[] gi : g) {
			Arrays.fill(gi, Integer.MAX_VALUE);
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int w = Integer.parseInt(line[2]);
			g[a][b] = g[b][a] = w;
		}
		in.close();
		
		long[][] dp = new long[1 << n][n];
		for (long[] d : dp) {
			Arrays.fill(d, Long.MAX_VALUE);
		}
		dp(dp, g, n);
		for (long[] d : dp) {
			System.out.println(Arrays.toString(d));
		}
		long minPath = Long.MAX_VALUE;
		for (int i = 0; i < n; i++) {
			minPath = Math.min(minPath, dp[(1 << n) - 1][n - i - 1]);
		}
		PrintWriter out = new PrintWriter(new File("salesman.out"));
		out.println(minPath == Long.MAX_VALUE ? -1 : minPath);
		out.close();
	}
}