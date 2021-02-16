import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

public class P_ChiSquared {
	
	public static void main(String[] args) throws IOException {
		new P_ChiSquared().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int k1 = Integer.parseInt(line[0]);
		int k2 = Integer.parseInt(line[1]);
		int n = Integer.parseInt(in.readLine());
		int[] amX1 = new int[k1];
		int[] amX2 = new int[k2];
		Map<Pair, Integer> amX1X2 = new HashMap<>();
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int x1 = Integer.parseInt(line[0]) - 1;
			int x2 = Integer.parseInt(line[1]) - 1;
			amX1[x1]++;
			amX2[x2]++;
			Pair key = new Pair(x1, x2);
			int prev = amX1X2.getOrDefault(key, 0);
			amX1X2.put(key, prev + 1);
		}
		
		double res = 0;
		for (Pair x1x2 : amX1X2.keySet()) {
			double e = 1.0 * amX1[x1x2.x1] * amX2[x1x2.x2] / n;
			double d = amX1X2.get(x1x2) - e;
			res += (d - e) * (d + e) / e;
		}
		System.out.printf(Locale.US, "%.10f%n", n + res);
	}
	
	private static class Pair {
		int x1;
		int x2;
		
		public Pair(int x1, int x2) {
			this.x1 = x1;
			this.x2 = x2;
		}
		
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof Pair)) return false;
			Pair pair = (Pair) o;
			return x1 == pair.x1 && x2 == pair.x2;
		}
		
		@Override
		public int hashCode() {
			return Objects.hash(x1, x2);
		}
	}
	
}
