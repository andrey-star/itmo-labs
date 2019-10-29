import java.io.*;

public class D_Parking {
	
	private static class DSU {
		
		private int[] parent;
		
		private DSU(int n) {
			parent = new int[n];
			for (int i = 0; i < n; i++) {
				parent[i] = i;
			}
		}
		
		private void union(int x, int y) {
			x = find(x);
			y = find(y);
			parent[x] = y;
		}
		
		private int find(int index) {
			if (parent[index] != index) {
				parent[index] = find(parent[index]);
			}
			return parent[index];
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
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("parking.in")));
		int n = Integer.parseInt(in.readLine());
		String[] line = in.readLine().trim().split(" +");
		int[] a = new int[n];
		for (int i = 0; i < a.length; i++) {
			a[i] = Integer.parseInt(line[i]) - 1;
		}
		DSU dsu = new DSU(n);
		PrintWriter out = new PrintWriter(new File("parking.out"));
		for (int place : a) {
			out.print(dsu.find(place) + 1 + " ");
			int preUnion = (n + place - 1) % n;
			if (dsu.find(preUnion) == dsu.find(place)) {
				dsu.union(preUnion, place);
			}
			dsu.union(place, (dsu.find(place) + 1) % n);
		}
		out.close();
	}
	
}
