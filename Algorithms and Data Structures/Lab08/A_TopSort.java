import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class A_TopSort {
	
	private static boolean dfs(List<Integer>[] g, int u, int[] color, List<Integer> topSort) {
		color[u] = 1;
		for (int v : g[u]) {
			if (color[v] == 0) {
				dfs(g, v ,color, topSort);
			}
			if (color[v] == 1) {
				return false;
			}
		}
		color[u] = 2;
		topSort.add(u);
		return true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("topsort.in")));
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
		}
		in.close();
		int[] color = new int[n];
		List<Integer> topSort = new ArrayList<>();
		boolean cycle = false;
		for (int i = 0; i < n; i++) {
			if (color[i] == 0) {
				if (!dfs(g, i, color, topSort)) {
					cycle = true;
					break;
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("topsort.out"));
		if (cycle) {
			out.println("-1");
		} else {
			Collections.reverse(topSort);
			for (int t : topSort) {
				out.print(t + 1 + " ");
			}
		}
		out.close();
	}
}
