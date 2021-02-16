import java.util.Arrays;
import java.util.function.BiFunction;

public class Kernel {
	
	public static double linear(double[] a, double[] b) {
		if (a.length != b.length) {
			throw new IllegalArgumentException("Kernel arguments of different dimensions");
		}
		double res = 0;
		for (int i = 0; i < a.length; i++) {
			res += a[i] * b[i];
		}
		return res;
	}
	
	private static double poly(double[] a, double[] b, int d) {
		return Math.pow(linear(a, b) + 1, d);
	}
	
	public static BiFunction<double[], double[], Double> poly(int d) {
		return (a, b) -> poly(a, b, d);
	}
	
	private static double rbf(double[] a, double[] b, double gamma) {
		if (a.length != b.length) {
			throw new IllegalArgumentException("Kernel arguments of different dimensions");
		}
		double[] diff = Arrays.copyOf(a, a.length);
		for (int i = 0; i < diff.length; i++) {
			diff[i] -= b[i];
		}
		return Math.exp(-gamma * Math.pow(linear(diff, diff), 2));
	}
	
	public static BiFunction<double[], double[], Double> rbf(double gamma) {
		return (a, b) -> rbf(a, b, gamma);
	}
	
	
}
