import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class D_Game {
	
	private static void dfs(List<Integer>[] g, int u, List<Integer> topSort, boolean[] used) {
		used[u] = true;
		for (int v : g[u]) {
			if (!used[v]) {
				dfs(g, v, topSort, used);
			}
		}
		topSort.add(u);
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("game.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int s = Integer.parseInt(line[2]) - 1;
		//noinspection unchecked
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
		}
		in.close();
		
		List<Integer> topSort = new ArrayList<>();
		dfs(g, s, topSort, new boolean[n]);
		boolean[] bad = new boolean[n];
		Arrays.fill(bad, true);
		mark: for (int i = 1; i < topSort.size(); i++) {
			int u = topSort.get(i);
			for (int v : g[u]) {
				if (bad[v]) {
					bad[u] = false;
					continue mark;
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("game.out"));
		out.println(bad[s] ? "Second player wins" : "First player wins");
		out.close();
	}
	
}
