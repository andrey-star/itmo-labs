import java.util.Map;
import java.util.function.DoubleUnaryOperator;

public class Kernel {
	
	public static final Map<String, DoubleUnaryOperator> kernels = Map.of(
			"uniform", Kernel::uniform,
			"triangular", Kernel::triangular,
			"epanechnikov", Kernel::epanechnikov,
			"quartic", Kernel::quartic,
			"triweight", Kernel::triweight,
			"tricube", Kernel::tricube,
			"gaussian", Kernel::gaussian,
			"cosine", Kernel::cosine,
			"logistic", Kernel::logistic,
			"sigmoid", Kernel::sigmoid
	);
	
	private static double uniform(double u) {
		if (Math.abs(u) >= 1) {
			return 0;
		}
		return 0.5;
	}
	
	private static double triangular(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 1 - Math.abs(u);
	}
	
	private static double epanechnikov(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 3.0 / 4.0 * (1 - u * u);
	}
	
	private static double quartic(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 15.0 / 16.0 * Math.pow(1 - u * u, 2);
	}
	
	private static double triweight(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 35.0 / 32.0 * Math.pow(1 - u * u, 3);
	}
	
	private static double tricube(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 70.0 / 81.0 * Math.pow(1 - Math.pow(Math.abs(u), 3), 3);
	}
	
	private static double gaussian(double u) {
		return 1 / Math.sqrt(2 * Math.PI) * Math.exp(-0.5 * u * u);
	}
	
	private static double cosine(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return Math.PI / 4 * Math.cos(Math.PI / 2 * u);
	}
	
	private static double logistic(double u) {
		return 1 / (Math.exp(u) + 2 + Math.exp(-u));
	}
	
	private static double sigmoid(double u) {
		return 2 / Math.PI * 1 / (Math.exp(u) + Math.exp(-u));
	}
	
}