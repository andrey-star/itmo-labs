import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class G_CombObjects {
	
	private static final int size = 7;
	private static final B b = new B();
	private static final Map<Long, Map<Long, Long>> c_n_k = new HashMap<>();
	private static int index = 0;
	private static String s;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		s = in.readLine();
		in.close();
		Comb comb = parseComb();
		long[] weight = comb.getWeight();
		for (long i : weight) {
			System.out.print(i + " ");
		}
		System.out.println();
	}
	
	private static Comb parseComb() {
		if (s.charAt(index) == 'B') {
			index++;
			return b;
		}
		if (s.charAt(index) == 'L') {
			index += 2;
			Comb comb = parseComb();
			index++;
			return new L(comb);
		}
		if (s.charAt(index) == 'S') {
			index += 2;
			Comb comb = parseComb();
			index++;
			return new S(comb);
		}
		index += 2;
		Comb first = parseComb();
		index++;
		Comb second = parseComb();
		index++;
		return new P(first, second);
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
	
	private interface Comb {
		long[] getWeight();
	}
	
	private static class B implements Comb {
		
		private static final long[] weight = new long[size];
		
		static {
			weight[1] = 1;
		}
		
		@Override
		public long[] getWeight() {
			return weight;
		}
		
		@Override
		public String toString() {
			return "B";
		}
	}
	
	private static class L implements Comb {
		
		private final Comb comb;
		private final long[] weight = new long[size];
		boolean counted = false;
		
		public L(Comb comb) {
			this.comb = comb;
		}
		
		@Override
		public long[] getWeight() {
			if (counted) {
				return weight;
			}
			long[] w = comb.getWeight();
			weight[0] = 1;
			for (int n = 1; n < size; n++) {
				for (int i = 1; i <= n; i++) {
					weight[n] += w[i] * weight[n - i];
				}
			}
			counted = true;
			return weight;
		}
		
		@Override
		public String toString() {
			return "L(" + comb + ")";
		}
	}
	
	private static class S implements Comb {
		
		private final Comb comb;
		private final long[] weight = new long[size];
		boolean counted = false;
		
		public S(Comb comb) {
			this.comb = comb;
		}
		
		@Override
		public long[] getWeight() {
			if (counted) {
				return weight;
			}
			long[] w = comb.getWeight();
			long[][] m = new long[size][size];
			for (int i = 0; i < size; i++) {
				m[0][i] = 1;
			}
			for (int n = 1; n < size; n++) {
				for (int k = 1; k < size; k++) {
					for (int i = 0; i <= n / k; i++) {
						m[n][k] += c(Math.max(w[k] + i - 1, 0), i) * m[n - i * k][k - 1];
					}
				}
			}
			for (int n = 0; n < size; n++) {
				weight[n] = m[n][n];
			}
			counted = true;
			return weight;
		}
		
		@Override
		public String toString() {
			return "S(" + comb + ")";
		}
	}
	
	private static class P implements Comb {
		
		private final Comb first;
		private final Comb second;
		private final long[] weight = new long[size];
		boolean counted = false;
		
		public P(Comb first, Comb second) {
			this.first = first;
			this.second = second;
		}
		
		@Override
		public long[] getWeight() {
			if (counted) {
				return weight;
			}
			long[] w = first.getWeight();
			long[] u = second.getWeight();
			for (int n = 0; n < size; n++) {
				for (int i = 0; i <= n; i++) {
					weight[n] += w[i] * u[n - i];
				}
			}
			counted = true;
			return weight;
		}
		
		@Override
		public String toString() {
			return "P(" + first + "," + second + ")";
		}
		
	}
}
