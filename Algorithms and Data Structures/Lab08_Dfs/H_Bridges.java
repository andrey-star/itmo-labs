import java.io.*;
import java.util.*;

public class H_Bridges {
	
	private static void dfs(List<Edge>[] g, int u, boolean[] used, int timer, int[] tin, int[] up, int[] p, Set<Integer> res) {
		timer++;
		tin[u] = timer;
		up[u] = timer;
		used[u] = true;
		for (Edge edge : g[u]) {
			int v = edge.a;
			if (!used[v]) {
				p[v] = u;
				dfs(g, v, used, timer, tin, up, p, res);
				up[u] = Math.min(up[u], up[v]);
				if (up[v] > tin[u]) {
					res.add(edge.index);
				}
			} else if (v != p[u]) {
				up[u] = Math.min(up[u], tin[v]);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("bridges.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		List<Edge>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(new Edge(b, i + 1));
			g[b].add(new Edge(a, i + 1));
		}
		in.close();
		
		boolean[] used = new boolean[n];
		int[] tin = new int[n];
		int[] up = new int[n];
		int[] p = new int[n];
		Arrays.fill(p, -1);
		
		int timer = 0;
		Set<Integer> res = new TreeSet<>();
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs(g, i, used, timer, tin, up, p, res);
			}
		}
		PrintWriter out = new PrintWriter(new File("bridges.out"));
		out.println(res.size());
		for (int edge : res) {
			out.println(edge);
		}
		out.close();
	}
	
	private static class Edge {
		int a;
		int index;
		
		Edge(int a, int index) {
			this.a = a;
			this.index = index;
		}
	}
}
