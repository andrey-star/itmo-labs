import java.util.Map;
import java.util.function.BiFunction;

public class Distance {
	
	public static final Map<String, BiFunction<double[], double[], Double>> dists = Map.of(
			"manhattan", Distance::manhattan,
			"euclidean", Distance::euclidean,
			"chebyshev", Distance::chebyshev
	
	);
	
	public static double manhattan(double[] a, double[] b) {
		return minkowski(a, b, 1);
	}
	
	public static double euclidean(double[] a, double[] b) {
		return minkowski(a, b, 2);
	}
	
	public static double chebyshev(double[] a, double[] b) {
		if (a.length != b.length) {
			throw new AssertionError();
		}
		double max = 0;
		for (int i = 0; i < a.length; i++) {
			max = Math.max(max, Math.abs(a[i] - b[i]));
		}
		return max;
	}
	
	public static double minkowski(double[] a, double[] b, int p) {
		if (a.length != b.length) {
			throw new AssertionError();
		}
		double sum = 0;
		for (int i = 0; i < a.length; i++) {
			sum += Math.pow(Math.abs(a[i] - b[i]), p);
		}
		return Math.pow(sum, 1.0 / p);
	}
	
	public static double cosine(double[] a, double[] b) {
		double nom = 0;
		double normA = 0;
		double normB = 0;
		for (int i = 0; i < a.length; i++) {
			nom += a[i] * b[i];
			normA += a[i] * a[i];
			normB += b[i] * b[i];
		}
		
		return nom / (Math.sqrt(normA) * Math.sqrt(normB));
	}
	
	public static double nrmse(double[] a, double[] b) {
		double sum = 0;
		double min = b[0];
		double max = b[0];
		for (int i = 0; i < a.length; i++) {
			min = Math.min(min, b[i]);
			max = Math.max(max, b[i]);
			sum += Math.pow(a[i] - b[i], 2);
		}
		return Math.sqrt(sum / a.length) / (max - min);
	}
	
	public static double smape(double[] a, double[] b) {
		double nom = 0;
		double den = 0;
		for (int i = 0; i < a.length; i++) {
			nom += Math.abs(a[i] - b[i]);
			den += Math.abs(a[i]) + Math.abs(b[i]);
		}
		return nom / den;
	}
}