import java.io.*;
import java.util.*;

public class G_Equivalence {
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("equivalence.in")));
		String[] line = in.readLine().trim().split(" +");
		int n1 = Integer.parseInt(line[0]);
		int m1 = Integer.parseInt(line[1]);
		int k1 = Integer.parseInt(line[2]);
		boolean[] t1 = new boolean[n1 + 1];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k1; i++) {
			t1[Integer.parseInt(line[i])] = true;
		}
		int[][] g1 = new int[n1 + 1]['z' - 'a' + 1];
//		for (int[] ints : g1) {
//			Arrays.fill(ints, -1);
//		}
		for (int i = 0; i < m1; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			char c = line[2].charAt(0);
			g1[a][c - 'a'] = b;
		}
		line = in.readLine().trim().split(" +");
		int n2 = Integer.parseInt(line[0]);
		int m2 = Integer.parseInt(line[1]);
		int k2 = Integer.parseInt(line[2]);
		boolean[] t2 = new boolean[n2 + 1];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k2; i++) {
			t2[Integer.parseInt(line[i])] = true;
		}
		int[][] g2 = new int[n2 + 1]['z' - 'a' + 1];
//		for (int[] ints : g2) {
//			Arrays.fill(ints, -1);
//		}
		for (int i = 0; i < m2; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			char c = line[2].charAt(0);
			g2[a][c - 'a'] = b;
		}
		PrintWriter out = new PrintWriter(new File("equivalence.out"));
		out.println(areEquivalent(n1, n2, 1, 1, g1, g2, t1, t2) ? "YES" : "NO");
		out.close();
	}
	
	private static boolean areEquivalent(int n1, int n2, int s1, int s2, int[][] g1, int[][] g2, boolean[] t1, boolean[] t2) {
		boolean[][] mark = new boolean[n1 + 1][n2 + 1];
		List<Pair> queue = new LinkedList<>();
		queue.add(new Pair(s1, s2));
		while (!queue.isEmpty()) {
			Pair uv = queue.remove(0);
			int u = uv.x;
			int v = uv.y;
			if (t1[u] != t2[v]) {
				return false;
			}
			mark[u][v] = true;
			for (int c = 0; c < 'z' - 'a' + 1; c++) {
				int u1 = g1[u][c];
				int v1 = g2[v][c];
				if (!mark[u1][v1]) {
					queue.add(new Pair(u1, v1));
				}
			}
		}
		return true;
	}
	
	static class Pair {
		final int x;
		final int y;
		
		Pair(int x, int y) {
			this.x = x;
			this.y = y;
		}
	}
}
