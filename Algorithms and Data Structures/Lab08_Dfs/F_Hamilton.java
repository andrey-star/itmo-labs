import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class F_Hamilton {
	
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
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("hamiltonian.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
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
		
		boolean[] used = new boolean[n];
		List<Integer> topSort = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs(g, i, topSort, used);
			}
		}
		Collections.reverse(topSort);
		
		boolean hasPath = true;
		for (int i = 0; i < topSort.size() - 1; i++) {
			int u = topSort.get(i);
			int v = topSort.get(i + 1);
			if (!g[u].contains(v)) {
				hasPath = false;
			}
		}
		PrintWriter out = new PrintWriter(new File("hamiltonian.out"));
		out.println(hasPath ? "YES" : "NO");
		out.close();
	}
}
