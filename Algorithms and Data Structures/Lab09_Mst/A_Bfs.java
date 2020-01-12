import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class A_Bfs {
	
	private static final int INF = Integer.MAX_VALUE;
	
	private static void bfs(List<Integer>[] g, int s, int[] d) {
		int n = g.length;
		for (int i = 0; i < n; i++) {
			d[i] = INF;
		}
		d[s] = 0;
		Queue<Integer> q = new ArrayDeque<>();
		boolean[] used = new boolean[n];
		q.add(s);
		used[s] = true;
		while (!q.isEmpty()) {
			int u = q.poll();
			for (int v : g[u]) {
				if (!used[v]) {
					d[v] = Math.min(d[u] + 1, d[v]);
					q.add(v);
					used[v] = true;
				}
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
//		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
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
		int[] d = new int[n];
		bfs(g, 0, d);
		for (int i = 0; i < n; i++) {
			System.out.print(d[i] + " ");
		}
	}
}
