import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class B_Condensation {
	
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
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("cond.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		//noinspection unchecked
		List<Integer>[] g = new ArrayList[n];
		//noinspection unchecked
		List<Integer>[] r = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
			r[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
			r[b].add(a);
		}
		in.close();
		
		boolean[] used = new boolean[n];
		List<Integer> topSort = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs1(g, i, used, topSort);
			}
		}
		
		used = new boolean[n];
		List<Integer> curComp = new ArrayList<>();
		int comps = 0;
		int[] comp = new int[n];
		for (int i = 0; i < n; i++) {
			int u = topSort.get(n - 1 - i);
			if (!used[u]) {
				dfs2(r, u, used, curComp);
				for (int v : curComp) {
					comp[v] = comps;
				}
				comps++;
				curComp.clear();
			}
		}
		
		PrintWriter out = new PrintWriter(new File("cond.out"));
		out.println(comps);
		for (int i : comp) {
			out.print(i + 1 + " ");
		}
		out.close();
	}
	
}
