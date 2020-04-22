import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class D_Formula {
	
	private static final long[] rPow = new long[11];
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int r = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		long[] p = new long[k + 1];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k + 1; i++) {
			p[i] = Integer.parseInt(line[i]);
		}
		in.close();
		
		rPow[0] = 1;
		for (int i = 1; i < rPow.length; i++) {
			rPow[i] = r * rPow[i - 1];
		}
		long[] sum = new long[0];
		for (int i = 0; i <= k; i++) {
			long[] f = f(k, i, p);
			sum = sum(sum, f);
		}
		print(sum, rPow[k] * fact(k), k);
	}
	
	private static long fact(long a) {
		long res = 1;
		for (int i = 2; i <= a; i++) {
			res *= i;
		}
		return res;
	}
	
	private static void print(long[] a, long div, int k) {
		for (int i = 0; i <= k; i++) {
			long nom = a[i];
			long gcd = gcd(Math.abs(nom), Math.abs(div));
			System.out.print(nom / gcd + "/" + div / gcd + " ");
		}
		System.out.println();
	}
	
	private static long gcd(long a, long b) {
		while (b != 0) {
			a %= b;
			long temp = a;
			a = b;
			b = temp;
		}
		return a;
	}
	
	private static long[] f(int k, int i, long[] p) {
		long[] res = new long[1];
		res[0] = 1;
		long[] f = new long[2];
		f[1] = 1;
		for (int j = 0; j < k; j++) {
			f[0] = j + 1 - i;
			res = prod(res, f);
		}
		return mul(res, rPow[k - i] * p[i]);
	}
	
	private static long[] prod(long[] p, long[] q) {
		int n = p.length + q.length;
		long[] prod = new long[n];
		for (int i = 0; i < n; i++) {
			long c = 0;
			for (int j = 0; j <= i; j++) {
				c += get(j, p) * get(i - j, q);
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
		}
		return sum;
	}
	
	private static long get(int i, long[] a) {
		return i < a.length ? a[i] : 0;
	}
	
	private static long[] mul(long[] a, long h) {
		int n = a.length;
		long[] mul = new long[n];
		for (int i = 0; i < n; i++) {
			mul[i] = a[i] * h;
		}
		return mul;
	}
}
