import java.io.*;
import java.util.Arrays;
import java.util.Random;

public class C_OptimalCode {
	
	private static long dp(int l, int r) {
		if (l > r) {
			return 0;
		}
		if (l == r) {
			dp[l][r] = 0;
			p[l][r] = l;
			return dp[l][r];
		}
		if (dp[l][r] != -1) {
			return dp[l][r];
		}
		int div = bestDivIndex(l, r);
		long res = dp(l, div) + dp(div + 1, r) + sum(l, r);
		dp[l][r] = res;
		return res;
	}
	
	private static int bestDivIndex(int l, int r) {
		if (p[l][r] != -1) {
			return p[l][r];
		}
		if (l == r) {
			p[l][r] = l;
			return l;
		}
		int lDiv = bestDivIndex(l, r - 1);
		int rDiv = bestDivIndex(l + 1, r);
		int minDiv = lDiv;
		long minDp = dp(l, minDiv) + dp(minDiv + 1, r);
		for (int i = lDiv; i <= rDiv; i++) {
			if (rDiv == r) {
				break;
			}
			if (dp(l, i) + dp(i + 1, r) < minDp) {
				minDiv = i;
				minDp = dp[l][i] + dp[i + 1][r];
			}
		}
		p[l][r] = minDiv;
		return minDiv;
	}
	
	private static long sum(int l, int r) {
		int res = sumPref[r];
		if (l > 0) {
			res -= sumPref[l - 1];
		}
		return res;
	}
	
	private static void fillSum() {
		sumPref[0] = a[0];
		for (int i = 1; i < a.length; i++) {
			sumPref[i] = sumPref[i - 1] + a[i];
		}
	}
	
	private static void code(int l, int r) {
		if (l == r) {
			return;
		}
		int div = p[l][r];
		for (int i = l; i <= r; i++) {
			codes[i].append(i <= div ? "0" : "1");
		}
		code(l, div);
		code(div + 1, r);
	}
	
	private static long[][] dp;
	private static int[] a;
	private static int[][] p;
	private static int[] sumPref;
	private static StringBuilder[] codes;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("optimalcode.in")));
		int n = Integer.parseInt(in.readLine());
		a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = Integer.parseInt(in.readLine());
		}
		in.close();
		p = new int[n][n];
		dp = new long[n][n];
		sumPref = new int[n];
		codes = new StringBuilder[n];
		for (int i = 0; i < n; i++) {
			codes[i] = new StringBuilder();
		}
		fillSum();
		for (long[] d : dp) {
			Arrays.fill(d, -1);
		}
		for (int[] pd : p) {
			Arrays.fill(pd, -1);
		}
		dp(0, n - 1);
		code(0, n - 1);
		PrintWriter out = new PrintWriter(new File("optimalcode.out"));
		out.println(dp[0][n - 1]);
		for (StringBuilder code : codes) {
			out.println(code);
		}
		out.close();
	}
	
}
