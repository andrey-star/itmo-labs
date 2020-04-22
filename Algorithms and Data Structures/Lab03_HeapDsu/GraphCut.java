import java.io.*;
import java.util.Arrays;
import java.util.LinkedList;

public class GraphCut {
	
	private static class DSU {
		
		private final int[] parent;
		
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
	
	static class Pair {
		int a;
		int b;
		
		Pair(int a, int b) {
			this.a = a;
			this.b = b;
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("cutting.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		
		for (int i = 0; i < m; i++) {
			in.readLine();
		}
		
		LinkedList<Pair> g = new LinkedList<>();
		String[] cmd = new String[k];
		int asked = 0;
		for (int i = 0; i < k; i++) {
			cmd[i] = in.readLine();
			line = cmd[i].trim().split(" +");
			if (line[0].equals("cut")) {
				int a = Integer.parseInt(line[1]);
				int b = Integer.parseInt(line[2]);
				g.add(new Pair(a, b));
			} else {
				asked++;
			}
		}
		DSU dsu = new DSU(n);
		String[] ans = new String[asked];
		int curAsk = ans.length - 1;
		for (int i = k - 1; i >= 0; i--) {
			line = cmd[i].trim().split(" +");
			if (line[0].equals("cut")) {
				Pair p = g.removeLast();
				dsu.union(p.a - 1, p.b - 1);
			} else if (line[0].equals("ask")) {
				int a = Integer.parseInt(line[1]);
				int b = Integer.parseInt(line[2]);
				if (dsu.find(a - 1) == dsu.find(b - 1)) {
					ans[curAsk] = "YES";
				} else {
					ans[curAsk] = "NO";
				}
				curAsk--;
			}
		}
		PrintWriter out = new PrintWriter(new File("cutting.out"));
		Arrays.stream(ans).forEach(out::println);
		out.close();
	}
	
}
