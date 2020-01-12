import java.io.*;
import java.util.ArrayList;

public class L_Matching {
	
	private static void dfs(ArrayList<Pair>[] g, long[][] dp, int u, int p) {
		for (Pair v : g[u]) {
			if (v.index != p) {
				dfs(g, dp, v.index, u);
				dp[u][0] += dp[v.index][2];
			}
		}
		
		for (Pair v : g[u]) {
			if (v.index != p) {
				dp[u][1] = Math.max(dp[u][1], dp[u][0] - dp[v.index][2] + dp[v.index][0] + v.w);
			}
		}
		dp[u][2] = Math.max(dp[u][0], dp[u][1]);
	}
	
	static class Pair {
		int index;
		int w;
		
		Pair(int x, int w) {
			this.index = x;
			this.w = w;
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("matching.in")));
		int n = Integer.parseInt(in.readLine());
		//noinspection unchecked
		ArrayList<Pair>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < n - 1; i++) {
			String[] line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int w = Integer.parseInt(line[2]);
			g[a].add(new Pair(b, w));
			g[b].add(new Pair(a, w));
		}
		long[][] dp = new long[n][3]; // 0 - sum, 1 - take u, 2 - max
		dfs(g, dp, 0, -1);
		PrintWriter out = new PrintWriter(new File("matching.out"));
		out.println(dp[0][2]);
		out.close();
		
	}
	
}