import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class D_Dijkstra {
	
	private static final int INF = Integer.MAX_VALUE;
	
	private static void dijkstra(List<Pair>[] g, int s, int[] d) {
		int n = g.length;
		Arrays.fill(d, INF);
		d[s] = 0;
		TreeSet<Pair> q = new TreeSet<>(Comparator.comparing((Pair p) -> p.w).thenComparing((Pair p) -> p.v));
		q.add(new Pair(s, 0));
		while (!q.isEmpty()) {
			int u = q.pollFirst().v;
			for (Pair p : g[u]) {
				int v = p.v;
				if (d[v] > d[u] + p.w) {
					q.remove(new Pair(v, d[v]));
					d[v] = d[u] + p.w;
					q.add(new Pair(v, d[v]));
				}
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		//noinspection unchecked
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
			g[b].add(new Pair(a, w));
		}
		int[] d = new int[n];
		dijkstra(g, 0, d);
		for (int i = 0; i < n; i++) {
			System.out.print(d[i] + " ");
		}
	}
	
	static class Pair {
		
		int v;
		int w;
		
		Pair(int v, int w) {
			this.v = v;
			this.w = w;
		}
	}
}