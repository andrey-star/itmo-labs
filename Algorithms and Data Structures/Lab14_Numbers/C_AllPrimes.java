import java.io.*;
import java.util.*;

public class C_AllPrimes {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int x = Integer.parseInt(line[1]);
		in.close();
		
		int h = 0;
		List<Integer> primes = linearSievePrimes((int) Math.sqrt(1e9));
		int blockSize = (int) 1e5;
		boolean[] isPrimeBlock = new boolean[blockSize];
		for (int prime : primes) {
			if (prime < blockSize && prime <= n) {
				h *= x;
				h += prime;
			}
		}
		for (int i = 1; i < n / blockSize + 1; i++) {
			int firstNumber = i * blockSize;
			for (int prime : primes) {
				int primeRelative = (firstNumber - 1) / prime;
				int inBlock = (primeRelative + 1) * prime - firstNumber;
				while (inBlock < blockSize) {
					isPrimeBlock[inBlock] = false;
					inBlock += prime;
				}
			}
			for (int j = 0; j < blockSize; j++) {
				int number = firstNumber + j;
				if (number > n) {
					break;
				}
				if (isPrimeBlock[j]) {
					h *= x;
					h += number;
				}
			}
			Arrays.fill(isPrimeBlock, true);
		}
		
		PrintWriter out = new PrintWriter(System.out);
		out.println(h);
		out.close();
	}
	
	private static List<Integer> linearSievePrimes(int n) {
		List<Integer> primes = new ArrayList<>();
		int[] s = new int[n + 1];
		for (int d = 2; d <= n; d++) {
			if (s[d] == 0) {
				s[d] = d;
				primes.add(d);
			}
			for (int j : primes) {
				if (j > s[d] || j * d > n) {
					break;
				}
				s[j * d] = j;
			}
		}
		return primes;
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
