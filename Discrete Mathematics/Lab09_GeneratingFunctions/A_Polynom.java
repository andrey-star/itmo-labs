import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class A_Polynom {
	
	private static final long MOD = 998_244_353;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]) + 1;
		int m = Integer.parseInt(line[1]) + 1;
		long[] p = new long[n];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			p[i] = Integer.parseInt(line[i]);
		}
		long[] q = new long[m];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < m; i++) {
			q[i] = Integer.parseInt(line[i]);
		}
		in.close();
		long[] sum = sum(p, q);
		print(sum);
		long[] prod = prod(p, q);
		print(prod);
		long[] div = div(p, q);
		print(div, 1000);
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
			System.out.print(get(i, a) + " ");
		}
		System.out.println();
	}
	
	private static long[] div(long[] p, long[] q) {
		int n = 1000;
		long[] div = new long[n];
		for (int i = 0; i < n; i++) {
			long bc = 0;
			for (int j = 1; j <= i; j++) {
				bc += get(j, q) * get(i - j, div);
				bc = mod(bc);
			}
			div[i] = (get(i, p) - bc) / q[0];
			div[i] = mod(div[i]);
		}
		return div;
	}
	
	private static long mod(long a) {
		if (a >= 0) {
			return a % MOD;
		}
		return a + MOD * (a / MOD + 1);
	}
	
	private static long[] prod(long[] p, long[] q) {
		int n = p.length + q.length;
		long[] prod = new long[n];
		for (int i = 0; i < n; i++) {
			long c = 0;
			for (int j = 0; j <= i; j++) {
				c += get(j, p) * get(i - j, q);
				c = mod(c);
			}
			prod[i] = c;
		}
		return prod;
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
	
	private static long get(int i, long[] a) {
		return i < a.length ? a[i] : 0;
	}
}
