import java.io.*;
import java.util.*;

public class K_BiconEdge {
	
	private static int maxColor = 0;
	
	private static void dfs(List<Integer>[] g, int u, boolean[] used, int timer, int[] tin, int[] up, int[] p) {
		timer++;
		tin[u] = timer;
		up[u] = timer;
		used[u] = true;
		for (int v : g[u]) {
			if (!used[v]) {
				p[v] = u;
				dfs(g, v, used, timer, tin, up, p);
				up[u] = Math.min(up[u], up[v]);
			} else if (v != p[u]) {
				up[u] = Math.min(up[u], tin[v]);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("bicone.in")));
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
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs(g, i, used, timer, tin, up, p);
			}
		}
		
		int[] color = new int[n];
		for (int i = 0; i < n; i++) {
			if (color[i] == 0) {
				paint(g, i, ++maxColor, color, tin, up);
			}
		}
		
		PrintWriter out = new PrintWriter(new File("bicone.out"));
		out.println(maxColor);
		Arrays.stream(color).forEach(c -> out.print(c + " "));
		out.close();
		
	}
	
	private static void paint(List<Integer>[] g, int u, int curColor, int[] color, int[] tin, int[] up) {
		color[u] = curColor;
		for (int v : g[u]) {
			if (color[v] == 0) {
				paint(g, v, up[v] > tin[u] ? ++maxColor : curColor, color, tin, up);
			}
		}
	}
}
