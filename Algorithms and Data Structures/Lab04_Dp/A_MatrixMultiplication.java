import java.io.*;
import java.util.Arrays;
import java.util.HashMap;

public class A_MatrixMultiplication {
	
	static class Pair {
		int a;
		int b;
		
		private Pair(int a, int b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public String toString() {
			return a + " " + b;
		}
	}
	
	private static void dp(long[][] dp, Pair[] mat, int n, int[][] div) {
		for (int size = 0; size < n; size++) {
			for (int i = 0; i < n - size; i++) {
				int j = i + size;
				if (i == j) {
					dp[i][j] = 0;
				} else if (i < j) {
					for (int k = i; k < j; k++) {
						if (dp[i][j] > dp[i][k] + dp[k + 1][j] + mat[i].a * mat[k].b * mat[j].b) {
							dp[i][j] = dp[i][k] + dp[k + 1][j] + mat[i].a * mat[k].b * mat[j].b;
							div[i][j] = k;
						}
					}
				}
			}
		}
	}
	
	private static HashMap<Integer, Pair> map;
	
	private static void findDivide(int[][] k, int left, int right) {
		if (left == right) {
			if (map.isEmpty()) {
				map.put(left, new Pair(0, 0));
				map.put(right, new Pair(0, 0));
			}
			return;
		}
		
		Pair l = map.getOrDefault(left, new Pair(0, 0));
		l.a++;
		map.put(left, l);
		
		Pair r = map.getOrDefault(right, new Pair(0, 0));
		r.b++;
		map.put(right, r);
		
		if (left + 1 == right) {
			return;
		}
		
		findDivide(k, left, k[left][right]);
		findDivide(k, k[left][right] + 1, right);
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("matrix.in")));
		int n = Integer.parseInt(in.readLine());
		Pair[] mat = new Pair[n];
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			mat[i] = new Pair(a, b);
		}
		in.close();
		
		long[][] dp = new long[n][n];
		int[][] division = new int[n][n];
		for (long[] d : dp) {
			Arrays.fill(d, Long.MAX_VALUE);
		}
		dp(dp, mat, n, division);
		map = new HashMap<>();
		findDivide(division, 0, n - 1);
		PrintWriter out = new PrintWriter(new File("matrix.out"));
//		PrintWriter out = new PrintWriter(System.out);
		for (int i = 0; i < map.size(); i++) {
			Pair p = map.get(i);
			for (int j = 0; j < p.a; j++) {
				out.print("(");
			}
			out.print("A");
			for (int j = 0; j < p.b; j++) {
				out.print(")");
			}
		}
		out.println();
		out.close();
		
	}
	
}
