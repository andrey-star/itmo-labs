import java.io.*;
import java.util.Arrays;

public class H_Permutations {
	
	private static void dp() {
		for (int i = 0; i < n; i++) {
			dp[1 << i][i] = 1;
		}
		for (int mask = 1; mask < 1 << n; mask++) {
			for (int i = 0; i < n; i++) { // last
				if (((mask >> i) & 1) == 1) { // if i in mask
					for (int j = 0; j < n; j++) { // all neighbours of last
						if (i != j && ((mask >> j) & 1) == 1 && g[i][j]) { // if j in mask and i <-> j
							dp[mask][i] += dp[mask - (1 << i)][j];
						}
					}
				}
			}
		}
	}
	
	private static void fill() {
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (gcd(a[i], a[j]) >= k) {
					g[i][j] = true;
				}
			}
		}
	}
	
	private static int gcd(int a, int b) {
		while (a != 0 && b != 0) {
			if (a > b) {
				a = a % b;
			} else {
				b = b % a;
			}
		}
		return a + b;
	}
	
	private static void gen(int[] pref, int length, long m, int last, int mask) {
		if (length == n) {
			Arrays.stream(pref).forEach(i -> out.print(i + " "));
			return;
		}
		for (int i = 0; i < n; i++) {
			if (((mask >> i) & 1) == 1 && (length == 0 || g[i][last])) {
				long skip = dp[mask][i];
				if (m - skip >= 0) {
					m -= skip;
				} else {
					pref[length] = a[i];
					last = i;
					break;
				}
			}
		}
		if (pref[length] == 0) {
			out.println(-1);
			return;
		}
		gen(pref, length + 1, m, last, mask - (1 << last));
	}
	
	private static int[] a;
	private static boolean[][] g;
	private static long[][] dp;
	private static int n, k;
	static PrintWriter out;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("perm.in")));
		String[] line = in.readLine().trim().split(" +");
		n = Integer.parseInt(line[0]);
		long m = Long.parseLong(line[1]) - 1;
		k = Integer.parseInt(line[2]);
		a = new int[n];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		Arrays.sort(a);
		in.close();
		g = new boolean[n][n];
		fill();
		dp = new long[1 << n][n];
		dp();
		out = new PrintWriter(new File("perm.out"));
		gen(new int[n], 0, m, -1, (1 << n) - 1);
		out.close();
	}
}