import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class L_BiconVertex {
	
	private static int maxColor = 0;
	
	private static boolean[] used;
	private static int[] color;
	private static int[] tin;
	private static int[] up;
	private static int[] p;
	private static int timer;
	private static List<Edge>[] g;
	
	private static void dfs(int u) {
		timer++;
		tin[u] = timer;
		up[u] = timer;
		used[u] = true;
		for (Edge e : g[u]) {
			int v = e.v;
			if (!used[v]) {
				p[v] = u;
				dfs(v);
				up[u] = Math.min(up[u], up[v]);
			} else if (v != p[u]) {
				up[u] = Math.min(up[u], tin[v]);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("biconv.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(new Edge(b, i));
			g[b].add(new Edge(a, i));
		}
		in.close();
		
		tin = new int[n];
		up = new int[n];
		used = new boolean[n];
		p = new int[n];
		Arrays.fill(p, -1);
		timer = 0;
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs(i);
			}
		}
		
		used = new boolean[n];
		color = new int[m];
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				paint(i, -1, ++maxColor);
			}
		}

//		PrintWriter out = new PrintWriter(new File("biconv.out"));
		PrintWriter out = new PrintWriter(System.out);
		out.println(maxColor);
		for (int i : color) {
			out.print(i + " ");
		}
		out.close();
		
	}
	
	private static void paint(int u, int p, int curColor) {
		used[u] = true;
		for (Edge e : g[u]) {
			int v = e.v;
			if (v == p) {
				continue;
			}
			if (!used[v]) {
				if (up[v] >= tin[u]) {
					int newColor = ++maxColor;
					color[e.index] = newColor;
					paint(v, u, newColor);
				} else {
					color[e.index] = curColor;
					paint(v, u, curColor);
				}
			} else if (tin[v] < tin[u]) {
				color[e.index] = curColor;
			}
		}
	}
	
	private static class Edge {
		int v;
		int index;
		
		Edge(int v, int index) {
			this.v = v;
			this.index = index;
		}
	}
}
