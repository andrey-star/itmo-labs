import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class I_DenseMst {
	
	private static int solve(List<Edge> e, int n) {
		DSU dsu = new DSU(n);
		int res = Integer.MAX_VALUE;
		for (int i = 0; i < e.size(); i++) {
			int minWeight = e.get(i).w;
			int maxWeight = minWeight;
			for (Edge edge : e) {
				if (edge.w >= minWeight) {
					maxWeight = Math.max(maxWeight, edge.w);
					dsu.union(edge.u, edge.v);
					if (dsu.isFull()) {
						res = Math.min(res, maxWeight - minWeight);
						break;
					}
				}
			}
			dsu.clear();
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
		g.sort(Comparator.comparing(e -> e.w));
		int res = solve(g, n);
		System.out.println(res == Integer.MAX_VALUE ? "NO" : "YES\n" + res);
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
	
	static private class DSU {
		
		private int[] parent;
		private int[] root;
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
		
		private int find(int x) {
			if (parent[x] != x) {
				parent[x] = find(parent[x]);
			}
			return parent[x];
		}
		
		private boolean isFull() {
			return comps == 1;
		}
		
		private void clear() {
			for (int i = 0; i < root.length; i++) {
				root[i] = 0;
			}
			for (int i = 0; i < parent.length; i++) {
				parent[i] = i;
				comps = parent.length;
			}
		}
	}
	
}