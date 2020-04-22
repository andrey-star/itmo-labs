import java.io.*;
import java.util.*;

public class B_Destroy {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("destroy.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		long s = Long.parseLong(line[2]);
		Edge[] edges = new Edge[m];
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			long c = Long.parseLong(line[2]);
			edges[i] = new Edge(a, b, c, i);
		}
		Arrays.sort(edges, Comparator.comparingLong((Edge e) -> e.c).reversed());
		
		DSU dsu = new DSU(n);
		boolean[] used = new boolean[m];
		int i = 0;
		while (!dsu.isConnected()) {
			Edge edge = edges[i];
			if (dsu.find(edge.a) != dsu.find(edge.b)) {
				dsu.union(edge.a, edge.b);
				used[i] = true;
			}
			i++;
		}
		
		List<Integer> res = new ArrayList<>();
		for (int j = m - 1; j >= 0; j--) {
			if (!used[j] && s - edges[j].c >= 0) {
				s -= edges[j].c;
				res.add(edges[j].index);
			}
		}
		
		PrintWriter out = new PrintWriter(new File("destroy.out"));
		out.println(res.size());
		res.forEach(e -> out.print(e + 1 + " "));
		out.close();
	}
	
	private static class Edge {
		final int a;
		final int b;
		final long c;
		final int index;
		
		Edge(int a, int b, long c, int index) {
			this.a = a;
			this.b = b;
			this.c = c;
			this.index = index;
		}
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
		
		private int find(int x) {
			if (parent[x] != x) {
				parent[x] = find(parent[x]);
			}
			return parent[x];
		}
		
		private boolean isConnected() {
			return comps == 1;
		}
	}
	
}
