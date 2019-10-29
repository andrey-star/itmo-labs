import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class N_Avia {
	
	private static void dfs1(List<Integer>[] g, int u, boolean[] used, List<Integer> topSort) {
		used[u] = true;
		for (int v : g[u]) {
			if (!used[v]) {
				dfs1(g, v, used, topSort);
			}
		}
		topSort.add(u);
	}
	
	private static void dfs2(List<Integer>[] g, int u, boolean[] used, List<Integer> comp) {
		used[u] = true;
		comp.add(u);
		for (int v : g[u]) {
			if (!used[v]) {
				dfs2(g, v, used, comp);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("avia.in")));
		int n = Integer.parseInt(in.readLine());
		
		int[][] g = new int[n][n];
		String[] line;
		int maxEdge = 0;
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			for (int j = 0; j < n; j++) {
				g[i][j] = Integer.parseInt(line[j]);
				maxEdge = Math.max(maxEdge, g[i][j]);
			}
		}
		in.close();
		
		int left = -1;
		int right = maxEdge;
		List<Integer>[] gg = new ArrayList[n];
		List<Integer>[] r = new ArrayList[n];
		List<Integer> comp = new ArrayList<>();
		List<Integer> topSort = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			gg[i] = new ArrayList<>();
			r[i] = new ArrayList<>();
		}
		while (left != right - 1) {
			
			int mid = (left + right) / 2;
			
			for (int i = 0; i < n; i++) {
				for (int j = 0; j < n; j++) {
					if (g[i][j] <= mid) {
						gg[i].add(j);
						r[j].add(i);
					}
				}
			}
			
			boolean[] used = new boolean[n];
			for (int i = 0; i < n; i++) {
				if (!used[i]) {
					dfs1(gg, i, used, topSort);
				}
			}
			used = new boolean[n];
			int comps = 0;
			for (int i = 0; i < n; i++) {
				int u = topSort.get(n - 1 - i);
				if (!used[u]) {
					dfs2(r, u, used, comp);
					comps++;
					comp.clear();
				}
			}
			
			if (comps > 1) {
				left = mid;
			} else {
				right = mid;
			}
			for (int i = 0; i < n; i++) {
				gg[i].clear();
				r[i].clear();
			}
			topSort.clear();
		}
		PrintWriter out = new PrintWriter(new File("avia.out"));
		out.println(right);
		out.close();
	}
}
