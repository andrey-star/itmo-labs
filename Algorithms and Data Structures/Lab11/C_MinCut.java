import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class C_MinCut {
	
	private static int dfs(int u, int t, int f, int[] edge, int[] d, List<Edge>[] g, int k) {
		if (u == t) {
			return f;
		}
		for (int i = edge[u]; i < g[u].size(); i++) {
			Edge e = g[u].get(i);
			edge[u] = i;
			if (d[e.v] == d[u] + 1 && ((long) e.f + (1 << k) <= e.c)) {
				int delta = dfs(e.v, t, Math.min(f, e.c - e.f), edge, d, g, k);
				if (delta > 0) {
					e.f += delta;
					e.back.f -= delta;
					return delta;
				}
			}
		}
		return 0;
	}
	
	private static int bfs(int s, int t, int[] d, List<Edge>[] g, int k) {
		Arrays.fill(d, -1);
		d[s] = 0;
		Queue<Integer> q = new ArrayDeque<>();
		q.add(s);
		while (!q.isEmpty()) {
			int u = q.remove();
			for (Edge e : g[u]) {
				if (d[e.v] < 0 && ((long) e.f + (1 << k) <= e.c)) {
					d[e.v] = d[u] + 1;
					q.add(e.v);
				}
			}
		}
		return d[t];
	}
	
	public static void main(String[] args) throws IOException {
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		//noinspection unchecked
		List<Edge>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int c = Integer.parseInt(line[2]);
			Edge f = new Edge(b, c);
			Edge s = new Edge(a, c);
			f.back = s;
			s.back = f;
			g[a].add(f);
			g[b].add(s);
		}
		in.close();
		
		int[] d = new int[n];
		for (int k = 30; k >= 0; k--) {
			while (bfs(0, n - 1, d, g, k) >= 0) {
				while (true) {
					if (dfs(0, n - 1, Integer.MAX_VALUE, new int[n], d, g, k) == 0) {
						break;
					}
				}
			}
		}
		List<Integer> minCut = new ArrayList<>();
		for (int i = 0; i < n; ++i) {
			if (d[i] != -1) {
				minCut.add(i);
			}
		}
		System.out.println(minCut.size());
		for (int u : minCut) {
			System.out.print(u + 1 + " ");
		}
	}
	
	private static class Edge {
		int v;
		Edge back;
		int c;
		int f;
		
		Edge(int v, int c) {
			this.v = v;
			this.c = c;
		}
	}
}
