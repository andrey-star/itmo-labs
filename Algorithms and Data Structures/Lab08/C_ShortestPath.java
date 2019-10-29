import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class C_ShortestPath {
	
	static class Pair {
		int v;
		int w;
		
		Pair(int v, int w) {
			this.v = v;
			this.w = w;
		}
	}
	
	private static void dfs(List<Pair>[] g, int u, List<Integer> topSort, boolean[] used) {
		used[u] = true;
		for (Pair p : g[u]) {
			if (!used[p.v]) {
				dfs(g, p.v, topSort, used);
			}
		}
		topSort.add(u);
	}
	
	public static int run(String path) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(path)));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int s = Integer.parseInt(line[2]) - 1;
		int t = Integer.parseInt(line[3]) - 1;
		List<Pair>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int w = Integer.parseInt(line[2]);
			g[a].add(new Pair(b, w));
		}
		in.close();
		
		List<Integer> topSort = new ArrayList<>();
		dfs(g, s, topSort, new boolean[n]);
		Collections.reverse(topSort);
		
		int[] d = new int[n];
		Arrays.fill(d, Integer.MAX_VALUE);
		d[s] = 0;
		for (int u : topSort) {
			for (Pair p : g[u]) {
				int v = p.v;
				d[v] = Math.min(d[v], d[u] + p.w);
			}
		}
		
		return d[t];
	}
	
	public static void main(String[] args) throws IOException {
		int res = run("shortpath.in");
		PrintWriter out = new PrintWriter(new File("shortpath.out"));
		if (res == Integer.MAX_VALUE) {
			out.println("Unreachable");
		} else {
			out.println(res);
		}
		out.close();
	}
}
