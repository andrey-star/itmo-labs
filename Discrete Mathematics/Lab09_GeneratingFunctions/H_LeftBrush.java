import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Arrays;

public class H_LeftBrush {
	
	private static final long MOD = 998_244_353;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int k = Integer.parseInt(line[0]);
		int n = Integer.parseInt(line[1]);
		in.close();
		
		long[][] pas = new long[k][k];
		for (int i = 0; i < k; i++) {
			pas[i][0] = 1;
			pas[i][i] = 1;
		}
		for (int i = 1; i < k; i++) {
			for (int j = 1; j < i; j++) {
				pas[i][j] = mod(pas[i - 1][j] + pas[i - 1][j - 1]);
			}
		}
		long[] p = new long[k / 2];
		for (int i = 0; i < p.length; i++) {
			p[i] = pas[k - 2 - i][i];
			if (i % 2 == 1) {
				p[i] = mod(-p[i]);
			}
		}
		long[] q = new long[(k + 1) / 2];
		for (int i = 0; i < q.length; i++) {
			q[i] = pas[k - 1 - i][i];
			if (i % 2 == 1) {
				q[i] = mod(-q[i]);
			}
		}
		long[] div = div(p, q, n);
		PrintWriter out = new PrintWriter(System.out);
		for (long l : div) {
			System.out.println(l);
		}
		out.close();
	}
	
	private static long[] div(long[] p, long[] q, int n) {
		long[] div = new long[n];
		for (int i = 0; i < n; i++) {
			long bc = 0;
			for (int j = 1; j <= i; j++) {
				bc += get(q, j) * get(div, i - j);
				bc = mod(bc);
			}
			div[i] = (get(p, i) - bc) / q[0];
			div[i] = mod(div[i]);
		}
		return div;
	}
	
	private static long get(long[] a, int i) {
		return i < a.length ? a[i] : 0;
	}
	
	private static long mod(long a) {
		a %= MOD;
		if (a < 0) {
			a += MOD;
		}
		return a;
	}
}
