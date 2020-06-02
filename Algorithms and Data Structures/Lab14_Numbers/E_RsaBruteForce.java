import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;

public class E_RsaBruteForce {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		int e = Integer.parseInt(in.readLine());
		int c = Integer.parseInt(in.readLine());
		in.close();
		solve(n, e, c);
	}
	
	private static void solve(long n, long e, long c) {
		long phi = getPhi(n);
		long d = BigInteger.valueOf(e).modInverse(BigInteger.valueOf(phi)).longValue();
		long m = BigInteger.valueOf(c).modPow(BigInteger.valueOf(d), BigInteger.valueOf(n)).longValue();
		System.out.println(m);
	}
	
	private static long getPhi(long n) {
		for (int i = 2; i < (int) Math.sqrt(n) + 1; i++) {
			if (n % i == 0) {
				return (i - 1) * (n / i - 1);
			}
		}
		throw new AssertionError();
	}
	
}
