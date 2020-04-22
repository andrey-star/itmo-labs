import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

public class E_QuasiPolynom {
	
	private static final Map<Long, Map<Long, Long>> c_n_k = new HashMap<>();
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int r = Integer.parseInt(in.readLine());
		int d = Integer.parseInt(in.readLine()) + 1;
		long[] a = new long[d];
		String[] line = in.readLine().trim().split(" +");
		for (int i = 0; i < d; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		in.close();
		long[] oneMinusS = new long[2];
		oneMinusS[0] = 1;
		oneMinusS[1] = -1;
		long[] oneMinusSPow = oneMinusS;
		long[] p = new long[1];
		p[0] = a[0];
		for (int i = 1; i < d; i++) {
			p = sum(prod(p, oneMinusS), prod(p(i), a[i]));
			oneMinusSPow = prod(oneMinusS, oneMinusSPow);
		}
		long rPow = 1;
		for (int i = 0; i < p.length; i++) {
			p[i] *= rPow;
			rPow *= r;
		}
		rPow = 1;
		for (int i = 0; i < oneMinusSPow.length; i++) {
			oneMinusSPow[i] *= rPow;
			rPow *= r;
		}
		print(p);
		print(oneMinusSPow);
	}
	
	private static long[] p(int m) {
		long[] a = new long[m + 1];
		for (int i = 0; i < m + 1; i++) {
			a[i] = (long) Math.pow(i, m);
		}
		long[] p = new long[m + 1];
		for (int k = 0; k < m + 1; k++) {
			p[k] = a[k];
			long actual = 0;
			for (int i = 1; i <= m + 1; i++) {
				int neg = (i + 1) % 2 == 0 ? 1 : -1;
				actual += get(a, k - i) * neg * c(m + 1, i);
			}
			p[k] = a[k] - actual;
		}
		return p;
	}
	
	private static long[] sum(long[] p, long[] q) {
		int size = Math.max(p.length, q.length);
		long[] sum = new long[size];
		for (int i = 0; i < size; i++) {
			sum[i] = get(p, i) + get(q, i);
		}
		return sum;
	}
	
	private static long[] prod(long[] p, long[] q) {
		int n = p.length + q.length;
		long[] prod = new long[n];
		for (int i = 0; i < n; i++) {
			long c = 0;
			for (int j = 0; j <= i; j++) {
				c += get(p, j) * get(q, i - j);
			}
			prod[i] = c;
		}
		return prod;
	}
	
	private static long[] prod(long[] p, long c) {
		int n = p.length;
		long[] prod = new long[n];
		for (int i = 0; i < n; i++) {
			prod[i] = p[i] * c;
		}
		return prod;
	}
	
	private static void print(long[] a) {
		int power = 0;
		for (int i = a.length - 1; i >= 0; i--) {
			if (a[i] != 0) {
				power = i;
				break;
			}
		}
		System.out.println(power);
		print(a, power + 1);
	}
	
	private static void print(long[] a, int amount) {
		for (int i = 0; i < amount; i++) {
			System.out.print(get(a, i) + " ");
		}
		System.out.println();
	}
	
	private static long get(long[] a, int i) {
		return 0 <= i && i < a.length ? a[i] : 0;
	}
	
	private static long c(long n, long k) {
		return c_n_k.computeIfAbsent(n, nn -> new HashMap<>())
		            .computeIfAbsent(k, kk -> {
			            if (kk > n) {
				            return 0L;
			            }
			            long res = 1;
			            for (int i = 1; i <= k; i++) {
				            res *= (n - i + 1);
				            res /= i;
			            }
			            return res;
		            });
	}
}
