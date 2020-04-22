import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class E_FordBellman {
	
	private static final long INF = Long.MAX_VALUE / 3 * 2;
	
	private static void dfs(List<Integer>[] g, int u, boolean[] used, boolean[] noShortestPath) {
		used[u] = true;
		for (int v : g[u]) {
			noShortestPath[v] = true;
			if (!used[v]) {
				dfs(g, v, used, noShortestPath);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int s = Integer.parseInt(line[2]) - 1;
		Edge[] g = new Edge[m];
		//noinspection unchecked
		List<Integer>[] gg = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			gg[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
//			int w = Integer.parseInt(line[2]);
			long w = Long.parseLong(line[2]);
			g[i] = new Edge(a, b, w);
			gg[a].add(b);
		}
		
		long[] d = new long[n];
		for (int i = 0; i < n; i++) {
			d[i] = INF;
		}
		d[s] = 0;
		for (int i = 0; i < n - 1; i++) {
			for (int j = 0; j < m; j++) {
				int u = g[j].u;
				int v = g[j].v;
				long w = g[j].w;
				if (d[u] == INF) {
					continue;
				}
				if (d[v] > d[u] + w) {
					d[v] = Math.max(-INF, d[u] + w);
				}
			}
		}
		
		boolean[] noShortestPath = new boolean[n];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < m; j++) {
				int u = g[j].u;
				int v = g[j].v;
				long w = g[j].w;
				if (d[u] == INF) {
					continue;
				}
				if (d[v] > d[u] + w) {
					noShortestPath[v] = true;
					d[v] = Math.max(-INF, d[u] + w);
				}
			}
		}
		
		boolean[] used = new boolean[n];
		for (int i = 0; i < n; i++) {
			if (!used[i] && noShortestPath[i]) {
				dfs(gg, i, used, noShortestPath);
			}
		}
		
		for (int i = 0; i < n; i++) {
			if (d[i] == INF) {
				System.out.println("*");
			} else if (noShortestPath[i]) {
				System.out.println("-");
			} else {
				System.out.println(d[i]);
			}
		}
		in.close();
	}
	
	static class Edge {
		
		int u;
		int v;
		long w;
		
		Edge(int u, int v, long w) {
			this.u = u;
			this.v = v;
			this.w = w;
		}
		
	}
}