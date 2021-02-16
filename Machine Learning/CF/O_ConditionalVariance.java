import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class O_ConditionalVariance {
	
	public static void main(String[] args) throws IOException {
		new O_ConditionalVariance().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int k = Integer.parseInt(in.readLine());
		int n = Integer.parseInt(in.readLine());
		Map<Integer, Double> probX = new HashMap<>();
		Map<Integer, Double> ey = new HashMap<>();
		int[] x = new int[n];
		double[] y = new double[n];
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			x[i] = Integer.parseInt(line[0]) - 1;
			y[i] = Integer.parseInt(line[1]);
			
			probX.putIfAbsent(x[i], 0.0);
			probX.put(x[i], probX.get(x[i]) + 1.0);
			ey.putIfAbsent(x[i], 0.0);
			ey.put(x[i], ey.get(x[i]) + y[i]);
		}
		probX.replaceAll((key, v) -> probX.get(key) / n);
		ey.replaceAll((key, v) -> ey.get(key) / n);
		double ey2 = 0;
		for (int i = 0; i < n; i++) {
			ey2 += y[i] * y[i] / n;
		}
		double res = 0;
		for (int xi : probX.keySet()) {
			res += ey.get(xi) * ey.get(xi) / probX.get(xi);
		}
		System.out.printf(Locale.US, "%.10f%n", ey2 - res);
	}
	
}
