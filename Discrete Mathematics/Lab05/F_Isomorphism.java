import java.io.*;
import java.util.Arrays;
import java.util.HashSet;

public class F_Isomorphism {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("isomorphism.in")));
		String[] line = in.readLine().trim().split(" +");
		int n1 = Integer.parseInt(line[0]);
		int m1 = Integer.parseInt(line[1]);
		int k1 = Integer.parseInt(line[2]);
		HashSet<Integer> t1 = new HashSet<>();
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k1; i++) {
			t1.add(Integer.parseInt(line[i]) - 1);
		}
		int[][] g1 = new int[n1]['z' - 'a' + 1];
		for (int[] ints : g1) {
			Arrays.fill(ints, -1);
		}
		for (int i = 0; i < m1; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			char c = line[2].charAt(0);
			g1[a][c - 'a'] = b;
		}
		line = in.readLine().trim().split(" +");
		int n2 = Integer.parseInt(line[0]);
		int m2 = Integer.parseInt(line[1]);
		int k2 = Integer.parseInt(line[2]);
		HashSet<Integer> t2 = new HashSet<>();
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k2; i++) {
			t2.add(Integer.parseInt(line[i]) - 1);
		}
		int[][] g2 = new int[n2]['z' - 'a' + 1];
		for (int[] ints : g2) {
			Arrays.fill(ints, -1);
		}
		for (int i = 0; i < m2; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			char c = line[2].charAt(0);
			g2[a][c - 'a'] = b;
		}
		PrintWriter out = new PrintWriter(new File("isomorphism.out"));
		out.println(areIsomorphic(0, 0, g1, g2, t1, t2, new boolean[Math.max(n1, n2)]) ? "YES" : "NO");
		out.close();
	}
	
	
	private static boolean areIsomorphic(int u, int v, int[][] g1, int[][] g2, HashSet<Integer> t1, HashSet<Integer> t2, boolean[] mark) {
		mark[u] = true;
		if (t1.contains(u) != t2.contains(v)) {
			return false;
		}
		boolean res = true;
		for (int i = 0; i < 'z' - 'a' + 1; i++) {
			int u1 = g1[u][i];
			if (u1 == -1) {
				continue;
			}
			int v1 = g2[v][i];
			if (v1 == -1) {
				return false;
			}
			if (t1.contains(u1) != t2.contains(v1)) {
				return false;
			}
			if (!mark[u1]){
				res &= areIsomorphic(u1, v1, g1, g2, t1, t2, mark);
			}
		}
		return res;
	}
	
}
