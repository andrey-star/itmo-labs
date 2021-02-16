import java.util.Map;
import java.util.function.BiFunction;

public class Distance {
	
	public static final Map<String, BiFunction<double[], double[], Double>> dists = Map.of(
			"manhattan", Distance::manhattan,
			"euclidean", Distance::euclidean,
			"chebyshev", Distance::chebyshev
	
	);
	
	private static double manhattan(double[] x1, double[] x2) {
		return minkowski(x1, x2, 1);
	}
	
	private static double euclidean(double[] x1, double[] x2) {
		return minkowski(x1, x2, 2);
	}
	
	private static double chebyshev(double[] x1, double[] x2) {
		if (x1.length != x2.length) {
			throw new AssertionError();
		}
		double max = 0;
		for (int i = 0; i < x1.length; i++) {
			max = Math.max(max, Math.abs(x1[i] - x2[i]));
		}
		return max;
	}
	
	private static double minkowski(double[] x1, double[] x2, int p) {
		if (x1.length != x2.length) {
			throw new AssertionError();
		}
		double sum = 0;
		for (int i = 0; i < x1.length; i++) {
			sum += Math.pow(Math.abs(x1[i] - x2[i]), p);
		}
		return Math.pow(sum, 1.0 / p);
	}
}