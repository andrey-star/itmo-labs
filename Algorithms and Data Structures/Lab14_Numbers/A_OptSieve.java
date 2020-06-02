import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Arrays;

public class A_OptSieve {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		int[] a = new int[n];
		int max = 0;
		for (int i = 0; i < n; i++) {
			a[i] = Integer.parseInt(in.readLine());
			max = Math.max(max, a[i]);
		}
		in.close();
		
		boolean[] s = optSieve(max);
		PrintWriter out = new PrintWriter(System.out);
		for (int i : a) {
			out.println(s[i] ? "YES" : "NO");
		}
		out.close();
	}
	
	private static boolean[] optSieve(int n) {
		boolean[] s = new boolean[n + 1];
		Arrays.fill(s, true);
		s[0] = false;
		s[1] = false;
		int sqrtMax = (int) Math.sqrt(n);
		for (int d = 2; d <= sqrtMax; d++) {
			if (s[d]) {
				int i = d * d;
				while (i <= n) {
					s[i] = false;
					i += d;
				}
			}
		}
		return s;
	}
	
}
