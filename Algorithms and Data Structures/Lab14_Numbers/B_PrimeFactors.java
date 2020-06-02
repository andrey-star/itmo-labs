import java.io.*;
import java.util.Arrays;

public class B_PrimeFactors {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		int max = (int) 1e6;
		
		int[] s = new int[max + 1];
		int[] primes = new int[(int) 1e5];
		Arrays.fill(primes, -1);
		linearSieve(max, s, primes);
		
		PrintWriter out = new PrintWriter(System.out);
		for (int i = 0; i < n; i++) {
			factor(Integer.parseInt(in.readLine()), s, out);
		}
		in.close();
		out.close();
	}
	
	private static void factor(int n, int[] s, PrintWriter out) {
		while (n != 1) {
			out.print(s[n] + " ");
			n /= s[n];
		}
		out.println();
	}
	
	private static void linearSieve(int n, int[] s, int[] primes) {
		int cur = 0;
		for (int d = 2; d <= n; d++) {
			if (s[d] == 0) {
				s[d] = d;
				primes[cur++] = d;
			}
			for (int j : primes) {
				if (j == -1 || j > s[d] || j * d > n) {
					break;
				}
				s[j * d] = j;
			}
		}
	}
	
}
