import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class M_Spearman {
	
	public static void main(String[] args) throws IOException {
		new M_Spearman().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		int[] x = new int[n];
		int[] y = new int[n];
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			x[i] = Integer.parseInt(line[0]);
			y[i] = Integer.parseInt(line[1]);
		}
		int[] rankX = rank(x);
		int[] rankY = rank(y);
		long rankSum = 0;
		for (int i = 0; i < n; i++) {
			rankSum += (long) (rankX[i] - rankY[i]) * (rankX[i] - rankY[i]);
		}
		double spearman = 1 - 6.0 / ((long) n * (n - 1) * (n + 1)) * rankSum;
		System.out.printf(Locale.US, "%.10f%n", spearman);
	}
	
	private int[] rank(int[] a) {
		List<Pair> pairs = IntStream.range(0, a.length)
				.mapToObj(i -> new Pair(a[i], i))
				.sorted(Comparator.comparing(p -> p.v))
				.collect(Collectors.toList());
		int[] rank = new int[a.length];
		int curRank = 0;
		for (int i = 0; i < pairs.size(); i++) {
			if (i > 0 && pairs.get(i - 1).v != pairs.get(i).v) {
				curRank++;
			}
			rank[pairs.get(i).index] = curRank;
		}
		return rank;
	}
	
	private static class Pair {
		double v;
		int index;
		
		public Pair(double v, int index) {
			this.v = v;
			this.index = index;
		}
	}
}
