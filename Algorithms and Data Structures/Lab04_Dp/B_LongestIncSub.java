import java.io.*;
import java.util.Arrays;

public class B_LongestIncSub {
	
	private static void dp(int[] dp, int[] dp2, int[] a, int n) {
		Arrays.fill(dp, Integer.MAX_VALUE);
		dp[0] = Integer.MIN_VALUE;
		for (int i = 0; i < n; i++) {
			int j = Arrays.binarySearch(dp, a[i]);
			if (j < 0) {
				j = -(j + 1);
			}
			dp[j] = Math.min(dp[j], a[i]);
			dp2[i] = j;
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("lis.in")));
		int n = Integer.parseInt(in.readLine());
		String[] line = in.readLine().trim().split(" +");
		in.close();
		int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		int[] dp = new int[n + 1];
		int[] dp2 = new int[n];
		dp(dp, dp2, a, n);
		int len = 0;
		for (int i = 0; i < dp.length; i++) {
			if (dp[i] < Integer.MAX_VALUE) {
				len = i;
			}
		}
		
		PrintWriter out = new PrintWriter(new File("lis.out"));
		out.println(len);
		int cur = Integer.MAX_VALUE;
		int[] res = new int[len];
		for (int i = n - 1; i >= 0; i--) {
			if (dp2[i] == len && a[i] <= cur) {
				cur = a[i];
				res[--len] = cur;
			}
		}
		for (int i : res) {
			out.print(i + " ");
		}
		out.close();
	}
	
}
