import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class E_Decomposition {
	
	private static int dfs(int u, int t, int f, int[] edge, int[] d, List<Edge>[] g, int k) {
		if (u == t) {
			return f;
		}
		for (int i = edge[u]; i < g[u].size(); i++) {
			Edge e = g[u].get(i);
			edge[u] = i;
			if (d[e.to] == d[u] + 1 && ((long) e.f + (1 << k) <= e.c)) {
				int delta = dfs(e.to, t, Math.min(f, e.c - e.f), edge, d, g, k);
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
				if (d[e.to] < 0 && ((long) e.f + (1 << k) <= e.c)) {
					d[e.to] = d[u] + 1;
					q.add(e.to);
				}
			}
		}
		return d[t];
	}
	
	private static Path dfsPath(int v, int t, int flow, List<Edge>[] g, boolean[] used) {
		if (v == t) {
			return new Path(flow);
		}
		used[v] = true;
		for (Edge e : g[v]) {
			if (!used[e.to] && e.f > 0) {
				Path result = dfsPath(e.to, t, Math.min(flow, e.f), g, used);
				if (result != null) {
					e.f -= result.cap;
					result.edges.add(e);
					return result;
				}
			}
		}
		return null;
	}
	
	private static String extractPath(int n, List<Edge>[] g) {
		boolean[] used = new boolean[n];
		Path path = dfsPath(0, n - 1, Integer.MAX_VALUE, g, used);
		if (path == null) {
			return null;
		}
		StringBuilder res = new StringBuilder();
		res.append(path.cap).append(" ").append(path.edges.size());
		Collections.reverse(path.edges);
		for (Edge edge : path.edges) {
			res.append(" ").append(edge.index + 1);
		}
		return res.toString();
	}
	
	public static void main(String[] args) throws IOException {
//      BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
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
			Edge f = new Edge(a, b, c, i);
			Edge s = new Edge(b, a, 0, -1);
			f.back = s;
			s.back = f;
			g[a].add(f);
			g[b].add(s);
		}
		in.close();
		
		long maxFlow = 0;
		int[] d = new int[n];
		for (int k = 30; k >= 0; k--) {
			while (bfs(0, n - 1, d, g, k) >= 0) {
				int[] ints = new int[n];
				while (true) {
					int flow = dfs(0, n - 1, Integer.MAX_VALUE, ints, d, g, k);
					if (flow == 0) {
						break;
					}
					maxFlow += flow;
				}
			}
		}
//      System.out.println(maxFlow);
		
		List<String> decomposition = new ArrayList<>();
		String path = extractPath(n, g);
		while (path != null) {
			decomposition.add(path);
			path = extractPath(n, g);
		}
		System.out.println(decomposition.size());
		for (String result : decomposition) {
			System.out.println(result);
		}
	}
	
	private static void print(Path result) {
		System.out.print(result.cap + " " + result.edges.size() + " ");
		for (Edge edge : result.edges) {
			System.out.print(edge.index + 1 + " ");
		}
		System.out.println();
	}
	
	private static class Edge {
		int from;
		int to;
		Edge back;
		int c;
		int f;
		int index;
		
		Edge(int u, int v, int c, int index) {
			this.from = u;
			this.to = v;
			this.c = c;
			this.index = index;
		}
		
		@Override
		public String toString() {
			return (from + 1) +
					"->" + (to + 1);
		}
	}
	
	private static class Path {
		
		List<Edge> edges;
		int cap;
		
		Path(int cap) {
			this.edges = new ArrayList<>();
			this.cap = cap;
		}
		
		@Override
		public String toString() {
			return edges + " cap " + cap;
		}
	}
}