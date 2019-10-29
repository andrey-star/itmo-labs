import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class J_Bipartite {
	
	private static boolean dfs(List<Integer>[] g, int u, int[] color) {
		for (int v : g[u]) {
			if (color[v] == 0) {
				color[v] = color[u] == 1 ? 2 : 1;
				dfs(g, v, color);
			} else if (color[v] == color[u]) {
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("bipartite.in")));
		String[] line = in.readLine().trim().split(" +");
		int n  = Integer.parseInt(line[0]);
		int m  = Integer.parseInt(line[1]);
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a  = Integer.parseInt(line[0]) - 1;
			int b  = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
			g[b].add(a);
		}
		in.close();
		
		int[] color = new int[n];
		boolean isBip = true;
		for (int i = 0; i < n; i++) {
			if (!dfs(g, i, color)) {
				isBip = false;
				break;
			}
		}
		PrintWriter out = new PrintWriter(new File("bipartite.out"));
		out.println(isBip ? "YES" : "NO");
		out.close();
	}
}
