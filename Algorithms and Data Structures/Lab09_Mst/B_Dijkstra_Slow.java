import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

public class B_Dijkstra_Slow {
	
	private static final long INF = Long.MAX_VALUE;
	
	private static long solve(int[][] g, int s, int t) {
		int n = g.length;
		long[] d = new long[n];
		Arrays.fill(d, INF);
		d[s] = 0;
		boolean[] used = new boolean[n];
		for (int i = 0; i < n; i++) {
			int u = -1;
			for (int j = 0; j < n; j++) {
				if (!used[j] && (u == -1 || d[j] < d[u])) {
					u = j;
				}
			}
			if (d[u] == INF) {
				break;
			}
			used[u] = true;
			for (int v = 0; v < n; v++) {
				if (g[u][v] >= 0) {
					d[v] = Math.min(d[v], d[u] + g[u][v]);
				}
			}
		}
		return d[t] == INF ? -1 : d[t];
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int s = Integer.parseInt(line[1]) - 1;
		int f = Integer.parseInt(line[2]) - 1;
		int[][] g = new int[n][n];
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			for (int j = 0; j < n; j++) {
				int w = Integer.parseInt(line[j]);
				g[i][j] = w;
			}
		}
		System.out.println(solve(g, s, f));
	}
	
}