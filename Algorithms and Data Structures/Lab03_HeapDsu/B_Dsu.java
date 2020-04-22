import java.io.*;

public class B_Dsu {
	
	private final int[] min;
	private final int[] max;
	private final int[] amount;
	private final int[] parent;
	private final int[] rank;
	
	private B_Dsu(int n) {
		min = new int[n];
		max = new int[n];
		amount = new int[n];
		parent = new int[n];
		rank = new int[n];
		for (int i = 0; i < n; i++) {
			min[i] = i;
			max[i] = i;
			amount[i] = 1;
			parent[i] = i;
		}
	}
	
	private void union(int x, int y) {
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
			min[x] = Math.min(min[x], min[y]);
			min[y] = min[x];
			max[x] = Math.max(max[x], max[y]);
			max[y] = max[x];
			amount[x] += amount[y];
			amount[y] = amount[x];
		}
	}
	
	private String get(int x) {
		return min[find(x)] + 1 + " " + (max[find(x)] + 1) + " " + amount[find(x)];
	}
	
	private int find(int x) {
		if (parent[x] != x) {
			parent[x] = find(parent[x]);
		}
		return parent[x];
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("dsu.in")));
		int n = Integer.parseInt(in.readLine());
		B_Dsu d = new B_Dsu(n);
		PrintWriter out = new PrintWriter(new File("dsu.out"));
		String lineS;
		while ((lineS = in.readLine()) != null) {
			String[] line = lineS.trim().split(" +");
			if (line[0].equals("union")) {
				int x = Integer.parseInt(line[1]) - 1;
				int y = Integer.parseInt(line[2]) - 1;
				d.union(x, y);
			} else {
				out.println(d.get(Integer.parseInt(line[1]) - 1));
			}
		}
		out.close();
	}
	
}
