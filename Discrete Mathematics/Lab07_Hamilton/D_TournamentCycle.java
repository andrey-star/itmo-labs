import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class D_TournamentCycle {
	
	private static void dfs(List<Integer>[] g, int u, boolean[] used, List<Integer> topSort) {
		used[u] = true;
		for (int v : g[u]) {
			if (!used[v]) {
				dfs(g, v, used, topSort);
			}
		}
		topSort.add(u);
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("guyaury.in")));
		int n = Integer.parseInt(in.readLine());
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < n; i++) {
			String s = in.readLine();
			for (int j = 0; j < i; j++) {
				if (s.charAt(j) == '1') {
					g[i].add(j);
				} else {
					g[j].add(i);
				}
			}
		}
		
		List<Integer> topSort = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			dfs(g, i, new boolean[n], topSort);
			Collections.reverse(topSort);
			int last = topSort.get(topSort.size() - 1);
			int first = topSort.get(0);
			if (g[last].contains(first)) {
				PrintWriter out = new PrintWriter(new File("guyaury.out"));
				out.println(topSort.stream().map(k -> k + 1 + " ").collect(Collectors.joining()));
				out.close();
				break;
			}
			topSort.clear();
		}
	}
	
}
