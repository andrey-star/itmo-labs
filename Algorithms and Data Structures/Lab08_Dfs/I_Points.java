import java.io.*;
import java.util.*;

public class I_Points {
	
	private static void dfs(List<Integer>[] g, int u, boolean[] used, int timer, int[] tin, int[] up, int[] p, Set<Integer> res) {
		timer++;
		tin[u] = timer;
		up[u] = timer;
		used[u] = true;
		int c = 0;
		for (int v : g[u]) {
			if (!used[v]) {
				c++;
				p[v] = u;
				dfs(g, v, used, timer, tin, up, p, res);
				up[u] = Math.min(up[u], up[v]);
				if (p[u] != -1 && up[v] >= tin[u]) {
					res.add(u + 1);
				}
				if (p[u] == -1 && c > 1) {
					res.add(u + 1);
				}
			} else if (v != p[u]) {
				up[u] = Math.min(up[u], tin[v]);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("points.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
			g[b].add(a);
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
		PrintWriter out = new PrintWriter(new File("points.out"));
		out.println(res.size());
		for (int edge : res) {
			out.println(edge);
		}
		out.close();
	}
}
