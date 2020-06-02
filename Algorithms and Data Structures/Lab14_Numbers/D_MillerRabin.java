import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Random;

public class D_MillerRabin {
	
	private static final Random random = new Random(4);
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		long[] a = new long[n];
		for (int i = 0; i < n; i++) {
			a[i] = Long.parseLong(in.readLine());
		}
		in.close();
		solve(a);
	}
	
	private static void solve(long[] a) {
		PrintWriter out = new PrintWriter(System.out);
		int t = 50;
		for (long l : a) {
			if (l == 1) {
				out.println("NO");
			} else if (l == 2) {
				out.println("YES");
			} else {
				out.println(millerRabinTest(l, t) ? "YES" : "NO");
			}
		}
		out.close();
	}
	
	private static void solve2(long[] a) {
		PrintWriter out = new PrintWriter(System.out);
		for (long l : a) {
			out.println(BigInteger.valueOf(l).isProbablePrime(50) ? "YES" : "NO");
		}
		out.close();
	}
	
	private static boolean millerRabinTest(long n, int t) {
		long s = n - 1;
		int k = 0;
		while (s % 2 == 0) {
			s /= 2;
			k++;
		}
		for (int i = 0; i < t; i++) {
			long x = (Math.abs(random.nextLong()) % (n - 2)) + 2;
			if (!check(n, x, s, k)) {
				return false;
			}
		}
		return true;
	}
	
	private static boolean check(long n, long x, long s, int k) {
		long y = modPow(x, s, n);
		for (int j = 0; j < k; j++) {
			long yy = modPow(y, 2, n);
			if (yy == 1 && y != n - 1 && y != 1) {
				return false;
			}
			y = yy;
		}
		return y == 1;
	}
	
	private static long modPow(long a, long n, long mod) {
		return BigInteger.valueOf(a).modPow(BigInteger.valueOf(n), BigInteger.valueOf(mod)).longValue();
	}
	
}