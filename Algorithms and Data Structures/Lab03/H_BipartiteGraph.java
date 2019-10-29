import java.io.*;

public class H_BipartiteGraph {
	
	private static class DSU {
		
		private int[] parent;
		private int[] rank;
		private boolean[] inv;
		
		private DSU(int n) {
			parent = new int[n];
			rank = new int[n];
			inv = new boolean[n];
			for (int i = 0; i < n; i++) {
				parent[i] = i;
			}
		}
		
		private void unite(int x, int y) {
			boolean same = color(x) == color(y);
			x = find(x);
			y = find(y);
			if (x != y) {
				if (rank[x] == rank[y]) {
					rank[x]++;
				}
				if (rank[x] < rank[y]) {
					parent[x] = y;
					inv[x] ^= same;
				} else {
					parent[y] = x;
					inv[y] ^= same;
				}
			}
		}
		
		private boolean color(int index) {
			boolean color = inv[index];
			if (parent[index] != index) {
				color ^= color(parent[index]);
			}
			return color;
		}
		
		private int find(int index) {
			return parent[index] != index ? find(parent[index]) : index;
		}
		
		@Override
		public String toString() {
			StringBuilder s = new StringBuilder();
			for (int i = 0; i < parent.length; i++) {
				s.append(find(i)).append(" ");
			}
			return s.toString();
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		DSU dsu = new DSU(n);
		int shift = 0;
		PrintWriter out = new PrintWriter(System.out);
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int op = Integer.parseInt(line[0]);
			int a = Integer.parseInt(line[1]) - 1;
			int b = Integer.parseInt(line[2]) - 1;
			a += shift;
			if (a >= n) {
				a -= n;
			}
			b += shift;
			if (b >= n) {
				b -= n;
			}
			if (op == 0) {
				dsu.unite(a, b);
			} else if (op == 1) {
				boolean same = (dsu.color(a) == dsu.color(b));
				out.println(same ? "YES" : "NO");
				shift += same ? 1 : 0;
				if (shift >= n) {
					shift -= n;
				}
			}
		}
		out.close();
	}
	
}
