import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class I_Generator {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int k = Integer.parseInt(line[0]);
		long n = Long.parseLong(line[1]) - 1;
		long[] a = new long[k];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		long[] c = new long[k];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			c[i] = Integer.parseInt(line[i]);
		}
		in.close();
		
		long[] q = new long[k + 1];
		q[0] = 1;
		for (int i = 1; i <= k; i++) {
			q[i] = -c[i - 1];
		}
		long[] p = p(a, c);
		
		while (n != 0) {
			long[] negate = negateOdd(q);
			q = even(prod(q, negate));
			p = prod(p, negate);
			if (n % 2 == 0) {
				p = even(p);
			} else {
				p = odd(p);
			}
			n /= 2;
		}
		System.out.println(mod(get(p, 0)));
	}
	
	private static long[] p(long[] a, long[] c) {
		int n = a.length;
		long[] p = new long[n];
		for (int k = 0; k < n; k++) {
			long actual = 0;
			for (int i = 1; i <= n; i++) {
				actual += get(c, i - 1) * get(a, k - i);
				actual = mod(actual);
			}
			p[k] = mod(a[k] - actual);
		}
		return p;
	}
	
	private static long[] even(long[] a) {
		return evenOdd(a, true);
	}
	
	private static long[] odd(long[] a) {
		return evenOdd(a, false);
	}
	
	private static long[] evenOdd(long[] a, boolean even) {
		long[] b = new long[a.length / 2];
		for (int i = 0; i < b.length; i++) {
			b[i] = get(a, 2 * i + (even ? 0 : 1));
		}
		return b;
	}
	
	
	private static long[] negateOdd(long[] a) {
		int n = a.length;
		long[] res = new long[n];
		System.arraycopy(a, 0, res, 0, n);
		for (int i = 1; i < n; i += 2) {
			res[i] = -res[i];
			res[i] = mod(res[i]);
		}
		return res;
	}
	
	private static long[] prod(long[] p, long[] q) {
		int n = p.length + q.length;
		long[] prod = new long[n];
		for (int i = 0; i < n; i++) {
			long c = 0;
			for (int j = 0; j <= i; j++) {
				c += get(p, j) * get(q, i - j);
				c = mod(c);
			}
			prod[i] = c;
		}
		return prod;
	}
	
	private static long mod(long a) {
		int MOD = 104857601;
		a %= MOD;
		if (a < 0) {
			a += MOD;
		}
		return a;
	}
	
	private static long get(long[] a, int i) {
		return 0 <= i && i < a.length ? mod(a[i]) : 0;
	}
	
}
