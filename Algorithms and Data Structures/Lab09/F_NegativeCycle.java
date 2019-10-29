import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class F_NegativeCycle {
	
	private static final int INF = 1_000_000_000;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		List<Edge> g = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			for (int j = 0; j < n; j++) {
				int w = Integer.parseInt(line[j]);
				if (w != INF) {
					g.add(new Edge(i, j, w));
				}
			}
		}
		int[] d = new int[n];
		int[] p = new int[n];
		Arrays.fill(p, -1);
		for (int i = 0; i < n - 1; i++) {
			for (Edge edge : g) {
				int u = edge.u;
				int v = edge.v;
				int w = edge.w;
				if (d[u] == INF) {
					continue;
				}
				if (d[v] > d[u] + w) {
					d[v] = Math.max(-INF, d[u] + w);
					p[v] = u;
				}
			}
		}
		for (Edge edge : g) {
			int u = edge.u;
			int v = edge.v;
			int w = edge.w;
			if (d[u] == INF) {
				continue;
			}
			if (d[v] > d[u] + w) {
				p[v] = u;
				for (int k = 0; k < n; k++) {
					v = p[v];
				}
				List<Integer> cycle = new ArrayList<>();
				u = v;
				do {
					cycle.add(u);
					u = p[u];
				} while (u != v);
				
				Collections.reverse(cycle);
				cycle.add(cycle.get(0));
				System.out.println("YES");
				System.out.println(cycle.size());
				for (int q : cycle) {
					System.out.print(q + 1 + " ");
				}
				return;
			}
		}
		System.out.println("NO");
	}
	
	static class Edge {
		
		int u;
		int v;
		int w;
		
		Edge(int u, int v, int w) {
			this.u = u;
			this.v = v;
			this.w = w;
		}
		
	}
	
}