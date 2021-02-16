import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

public class Q_ConditionalEntropy {
	
	public static void main(String[] args) throws IOException {
		new Q_ConditionalEntropy().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int kx = Integer.parseInt(line[0]);
		int ky = Integer.parseInt(line[1]);
		int n = Integer.parseInt(in.readLine());
		int[] amX = new int[kx];
		Map<Pair, Integer> amXY = new HashMap<>();
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int x = Integer.parseInt(line[0]) - 1;
			int y = Integer.parseInt(line[1]) - 1;
			amX[x]++;
			Pair key = new Pair(x, y);
			int prev = amXY.getOrDefault(key, 0);
			amXY.put(key, prev + 1);
		}
		
		double res = 0;
		for (Pair xy : amXY.keySet()) {
			res += -1.0 * amXY.get(xy) / n * Math.log(1.0 * amXY.get(xy) / amX[xy.x]);
		}
		System.out.printf(Locale.US, "%.10f%n", res);
	}
	
	private static class Pair {
		int x;
		int y;
		
		public Pair(int x, int y) {
			this.x = x;
			this.y = y;
		}
		
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof Pair)) return false;
			Pair pair = (Pair) o;
			return x == pair.x && y == pair.y;
		}
		
		@Override
		public int hashCode() {
			return Objects.hash(x, y);
		}
	}
	
}
