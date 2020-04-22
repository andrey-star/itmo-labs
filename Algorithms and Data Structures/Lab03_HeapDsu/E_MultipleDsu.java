import java.io.*;

public class E_MultipleDsu {
	
	private static class DSU {
		private final int[] parent;
		private final int[] rank;
		private final int[] firstNextSet;
		
		private DSU(int n) {
			parent = new int[n];
			rank = new int[n];
			firstNextSet = new int[n];
			for (int i = 0; i < n; i++) {
				parent[i] = i;
				firstNextSet[i] = i < n - 1 ? i + 1 : i;
			}
		}
		
		private void mulUnite(int x, int y) {
			while (x < y) {
				int temp = findFirstNextSet(x);
				if (temp > y) {
					return;
				}
				unite(x, temp);
				x = temp;
			}
		}
		
		private void unite(int x, int y) {
			x = find(x);
			y = find(y);
			if (x != y) {
				if (rank[x] == rank[y]) {
					rank[x]++;
				}
				if (rank[x] <= rank[y]) {
					parent[x] = y;
				} else {
					parent[y] = x;
				}
			}
		}
		
		private int find(int index) {
			if (parent[index] != index) {
				parent[index] = find(parent[index]);
			}
			return parent[index];
		}
		
		private int findFirstNextSet(int index) {
			if (firstNextSet[index] != firstNextSet.length - 1 && find(firstNextSet[index]) == find(index)) {
				firstNextSet[index] = findFirstNextSet(firstNextSet[index]);
			}
			return firstNextSet[index];
		}
	}
	
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("restructure.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int q = Integer.parseInt(line[1]);
		DSU dsu = new DSU(n);
		PrintWriter out = new PrintWriter(new File("restructure.out"));
		for (int i = 0; i < q; i++) {
			line = in.readLine().trim().split(" +");
			int type = Integer.parseInt(line[0]);
			int x = Integer.parseInt(line[1]) - 1;
			int y = Integer.parseInt(line[2]) - 1;
			if (type == 1) {
				dsu.unite(x, y);
			} else if (type == 2) {
				dsu.mulUnite(x, y);
			} else if (type == 3) {
				out.println(dsu.find(x) == dsu.find(y) ? "YES" : "NO");
			}
		}
		out.close();
	}
	
}
