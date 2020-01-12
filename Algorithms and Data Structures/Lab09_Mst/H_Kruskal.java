import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class H_Kruskal {
	
	private static int kruskal(List<Edge> g, int n) {
		g.sort(Comparator.comparing(e -> e.w));
		int[] tree = new int[n];
		for (int i = 0; i < n; i++) {
			tree[i] = i;
		}
		int res = 0;
		for (Edge e : g) {
			int idU = tree[e.u];
			int idV = tree[e.v];
			if (idU != idV) {
				res += e.w;
				for (int i = 0; i < n; i++) {
					if(tree[i] == idV) {
						tree[i] = idU;
					}
				}
			}
		}
		return res;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		List<Edge> g = new ArrayList<>();
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int w = Integer.parseInt(line[2]);
			g.add(new Edge(a, b, w));
			g.add(new Edge(b, a, w));
		}
		System.out.println(kruskal(g, n));
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