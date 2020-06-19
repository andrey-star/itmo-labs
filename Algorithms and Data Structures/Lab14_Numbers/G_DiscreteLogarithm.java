import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class G_DiscreteLogarithm {
	
	private static final Random random = new Random(4);
	
	public static void main(String[] args) throws IOException {
//		for (int i = 0; i < 10; i++) {
//			test();
//		}
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		long a = Long.parseLong(line[0]);
		long b = Long.parseLong(line[1]);
		long n = Long.parseLong(line[2]);
		solve(a, b, n);
	}
	
	private static void test() {
		long a = Math.abs(random.nextLong()) % ((long) 1e12 + 1);
		long n = Math.abs(random.nextLong()) % ((long) 1e12) + 1;
		int res = random.nextInt(Integer.MAX_VALUE);
		long b = pow(a, res, n);
		System.out.println(a + " " + b + " " + n);
		solve(a, b, n);
	}
	
	private static void solve(long a, long b, long n) {
		long sqrt = (int) Math.sqrt(n) + 1;
		long aToSqrtModN = pow(a, sqrt, n);
		
		Map<Long, Long> logs = new HashMap<>();
		long cur = 1;
		long log = 1;
		while (log <= sqrt) {
			cur = prod(cur, aToSqrtModN, n);
			logs.putIfAbsent(cur, log);
			log++;
		}
		long res = -1;
		cur = b;
		for (int i = 0; i <= sqrt; i++) {
			if (logs.containsKey(cur)) {
				log = logs.get(cur);
				long current = log * sqrt - i;
				if (current < n) {
					res = current;
					break;
				}
			}
			cur = prod(cur, a ,n);
		}
		
		long actual = pow(a, res, n);
		if (res != -1 && actual != b % n) {
			res = -1;
		}
		System.out.println(res);
	}
	
	private static long pow(long a, long n, long m) {
		long aToN = 1;
		while (n != 0) {
			if (n % 2 == 1) {
				aToN = prod(aToN, a, m);
				n--;
			} else {
				a = prod(a, a, m);
				n /= 2;
			}
		}
		return aToN;
	}
	
	private static long prod(long a, long n, long m) {
		long aTimesN = 0;
		while (n != 0) {
			if (n % 2 == 1) {
				aTimesN += a;
				aTimesN %= m;
				n--;
			} else {
				a *= 2;
				a %= m;
				n /= 2;
			}
		}
		return aTimesN;
	}
	
}
