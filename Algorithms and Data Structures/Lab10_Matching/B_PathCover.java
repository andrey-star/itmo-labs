import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class B_PathCover {
	
	private static boolean dfs(int u, List<Integer>[] g, boolean[] used, int[] a) {
		used[u] = true;
		for (int v : g[u]) {
			if (a[v] == -1) {
				a[v] = u;
				return true;
			}
			if (!used[a[v]] && dfs(a[v], g, used, a)) {
				a[v] = u;
				return true;
			}
		}
		return false;
	}
	
	public static void main(String[] args) throws IOException {
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		//noinspection unchecked
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < k; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
		}
		boolean[] used = new boolean[n];
		int[] matching = new int[n];
		Arrays.fill(matching, -1);
		for (int i = 0; i < n; i++) {
			Arrays.fill(used, false);
			dfs(i, g, used, matching);
		}
		DSU dsu = new DSU(n);
		for (int i = 0; i < n; i++) {
			if (matching[i] != -1) {
				dsu.union(i, matching[i]);
			}
		}
		System.out.println(dsu.getComps());
	}
	
	static private class DSU {
		
		private final int[] parent;
		private final int[] root;
		private int comps = 0;
		
		private DSU(int n) {
			parent = new int[n];
			root = new int[n];
			for (int i = 0; i < n; i++) {
				parent[i] = i;
				comps = n;
			}
		}
		
		private void union(int x, int y) {
			x = find(x);
			y = find(y);
			if (x != y) {
				comps--;
				if (root[x] == root[y]) {
					root[x]++;
				}
				if (root[x] <= root[y]) {
					parent[x] = y;
				} else {
					parent[y] = x;
				}
			}
		}
		
		int getComps() {
			return comps;
		}
		
		private int find(int x) {
			if (parent[x] != x) {
				parent[x] = find(parent[x]);
			}
			return parent[x];
		}
	}
}
