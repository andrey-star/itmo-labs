import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;

public class B_Polynom2 {
	
	public static final long MOD = 998_244_353;
	private static final Map<Long, Long> fact = new HashMap<>();
	private static final long[] pow4 = new long[105];
	
	static {
		pow4[0] = 1;
		for (int i = 1; i < pow4.length; i++) {
			pow4[i] = mod(pow4[i - 1] * 4);
		}
	}
	
	private static long fact(long a) {
		return fact.computeIfAbsent(a, aa -> {
			long res = 1;
			for (int i = 2; i <= a; i++) {
				res *= i;
				res = mod(res);
			}
			return res;
		});
	}
	
	private static void print(long[] a, int amount) {
		for (int i = 0; i < amount; i++) {
			System.out.print(get(i, a) + " ");
		}
		System.out.println();
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]) + 1;
		int m = Integer.parseInt(line[1]);
		long[] p = new long[n];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			p[i] = Integer.parseInt(line[i]);
		}
		in.close();
		print(res(p, m, B_Polynom2::sqrt), m);
		print(res(p, m, B_Polynom2::e), m);
		print(res(p, m, B_Polynom2::ln), m);
	}
	
	private static long[] res(long[] p, int m, BiFunction<long[], Integer, long[]> ithTerm) {
		long[] res = new long[1];
		long[] p_to_i = new long[1];
		p_to_i[0] = 1;
		for (int i = 0; i <= m - 1; i++) {
			res = sum(res, ithTerm.apply(p_to_i, i));
			p_to_i = prod(p_to_i, p, m);
		}
		return res;
	}
	
	private static long[] sqrt(long[] a, int n) {
		int neg = (n % 2 == 1) ? -1 : 1;
		long nom = mod(fact(2 * n) * neg);
		long den = mul(pow4[n], fact(n), fact(n), 1 - 2 * n);
		return div(mul(a, nom), den);
	}
	
	private static long[] e(long[] a, int n) {
		return div(a, fact(n));
	}
	
	private static long[] ln(long[] a, int n) {
		if (n == 0) {
			return new long[0];
		}
		long[] res = div(a, n);
		if (n % 2 == 0) {
			negate(res);
		}
		return res;
	}
	
	private static long[] mul(long[] a, long h) {
		int n = a.length;
		long[] mul = new long[n];
		for (int i = 0; i < n; i++) {
			mul[i] = mod(a[i] * h);
		}
		return mul;
	}
	
	private static long mul(long... n) {
		long res = n[0];
		for (int i = 1; i < n.length; i++) {
			res = mod(res * n[i]);
		}
		return res;
	}
	
	private static long[] sum(long[] p, long[] q) {
		int size = Math.max(p.length, q.length);
		long[] sum = new long[size];
		for (int i = 0; i < size; i++) {
			sum[i] = get(i, p) + get(i, q);
			sum[i] = mod(sum[i]);
		}
		return sum;
	}
	
	private static long[] prod(long[] p, long[] q, int m) {
		long[] prod = new long[m];
		for (int i = 0; i < m; i++) {
			long c = 0;
			for (int j = 0; j <= i; j++) {
				c += get(j, p) * get(i - j, q);
				c = mod(c);
			}
			prod[i] = c;
		}
		return prod;
	}
	
	private static long[] div(long[] p, long a) {
		int n = p.length;
		long[] div = new long[n];
		for (int i = 0; i < n; i++) {
			div[i] = div(p[i], a);
		}
		return div;
	}
	
	private static long div(long a, long b) {
		return mod(a * rev(b));
	}
	
	private static long rev(long a) {
		Pair p = new Pair(0, 0);
		long g = gcd(a, MOD, p);
		if (g != 1) {
			throw new AssertionError();
		} else {
			return mod(p.x);
		}
	}
	
	private static void negate(long[] a) {
		for (int i = 0; i < a.length; i++) {
			a[i] = mod(-a[i]);
		}
	}
	
	private static long gcd(long a, long b, Pair p) {
		if (a == 0) {
			p.x = 0;
			p.y = 1;
			return b;
		}
		Pair p1 = new Pair(0, 0);
		long d = gcd(b % a, a, p1);
		p.x = mod(p1.y - (b / a) * p1.x);
		p.y = mod(p1.x);
		return mod(d);
	}
	
	private static long mod(long a) {
		a %= MOD;
		if (a < 0) {
			a += MOD;
		}
		return a;
	}
	
	private static long get(int i, long[] a) {
		return i < a.length ? a[i] : 0;
	}
	
	private static class Pair {
		
		long x;
		long y;
		
		public Pair(long x, long y) {
			this.x = x;
			this.y = y;
		}
	}
}
