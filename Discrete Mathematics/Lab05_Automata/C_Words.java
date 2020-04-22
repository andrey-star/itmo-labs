import java.io.*;
import java.util.*;

public class C_Words {

	
	public static void main(String[] args) throws IOException {
		solve();
	}
	
	private static void solve() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("problem3.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		int[] term = new int[k];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			term[i] = Integer.parseInt(line[i]) - 1;
		}
		//noinspection unchecked
		List<Integer>[] g = new ArrayList[n];
		//noinspection unchecked
		List<Integer>[] gRev = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
			gRev[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
			gRev[b].add(a);
		}
		boolean[] reachableFromStart = new boolean[n];
		List<Integer> topSort = new ArrayList<>();
		markReachable(0, g, reachableFromStart, topSort);
		PrintWriter out = new PrintWriter(new File("problem3.out"));
		boolean isInf = false;
		int[] color = new int[n];
		for (int i : term) {
			if (isInfinite(i, gRev, color, reachableFromStart)) {
				isInf = true;
				break;
			}
		}
		int sum = 0;
		if (isInf) {
			sum = -1;
		} else {
			Collections.reverse(topSort);
			int[] dp = new int[n];
			dp[0] = 1;
			for (int integer : topSort) {
				for (int v : g[integer]) {
					dp[v] += dp[integer];
					dp[v] %= ((int) 1e9 + 7);
				}
			}
			for (int i : term) {
				sum += dp[i];
				sum %= (int) 1e9 + 7;
			}
		}
		out.println(sum);
		out.close();
		
	}
	
	private static void markReachable(int u, List<Integer>[] g, boolean[] mark, List<Integer> topSort) {
		mark[u] = true;
		for (int v : g[u]) {
			if (!mark[v]) {
				markReachable(v, g, mark, topSort);
			}
		}
		topSort.add(u);
	}
	
	private static boolean isInfinite(int u, List<Integer>[] g, int[] color, boolean[] reachableFromStart) {
		if (color[u] == 2) {
			return false;
		}
		color[u] = 1;
		for (int v : g[u]) {
			if (color[v] == 0) {
				boolean isInf = isInfinite(v, g, color, reachableFromStart);
				if (isInf && reachableFromStart[v]) {
					return true;
				}
			}
			if (color[v] == 1 && reachableFromStart[v]) {
				return true;
			}
		}
		color[u] = 2;
		return false;
	}
	
}
