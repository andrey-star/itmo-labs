import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.DoubleUnaryOperator;

public class C_NonParametricRegression {
	
	private final Map<String, BiFunction<int[], int[], Double>> dists = Map.of(
			"manhattan", this::manhattan,
			"euclidean", this::euclidean,
			"chebyshev", this::chebyshev
	
	);
	private final Map<String, DoubleUnaryOperator> kernels = Map.of(
			"uniform", this::uniform,
			"triangular", this::triangular,
			"epanechnikov", this::epanechnikov,
			"quartic", this::quartic,
			"triweight", this::triweight,
			"tricube", this::tricube,
			"gaussian", this::gaussian,
			"cosine", this::cosine,
			"logistic", this::logistic,
			"sigmoid", this::sigmoid
	);
	int n, m;
	int[][] f;
	int[] y;
	String distType, kernelType, windowType;
	int h, k;
	
	public static void main(String[] args) throws IOException {
		new C_NonParametricRegression().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		n = Integer.parseInt(line[0]);
		m = Integer.parseInt(line[1]);
		f = new int[n][m];
		y = new int[n];
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			for (int j = 0; j < m + 1; j++) {
				int d = Integer.parseInt(line[j]);
				if (j < m) {
					f[i][j] = d;
				} else {
					y[i] = d;
				}
			}
		}
		int[] q = new int[m];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < m; i++) {
			q[i] = Integer.parseInt(line[i]);
		}
		distType = in.readLine();
		kernelType = in.readLine();
		windowType = in.readLine();
		h = Integer.parseInt(in.readLine());
		k = h;
		in.close();
		
		System.out.println(solve(q));
	}
	
	private double solve(int[] q) {
		return nonParamReg(q);
	}
	
	private double nonParamReg(int[] q) {
		double[] distQ = dist(q);
		double[] distQSorted = Arrays.copyOf(distQ, n);
		Arrays.sort(distQSorted);
		if ((windowType.equals("fixed") && h == 0) || (windowType.equals("variable") && distQSorted[k] == 0)) {
			return averageY(distQ);
		}
		double nom = 0;
		double den = 0;
		for (int i = 0; i < n; i++) {
			int yi = y[i];
			double w = weight(i, distQ, distQSorted);
			nom += yi * w;
			den += w;
		}
		if (den == 0) {
			return averageY(distQ);
		}
		return nom / den;
	}
	
	private double averageY(double[] distQ) {
		double sum = 0;
		double am = 0;
		for (int i = 0; i < n; i++) {
			if (distQ[i] < 1e-10) {
				sum += y[i];
				am++;
			}
		}
		if (am != 0) {
			return sum / am;
		}
		sum = 0;
		for (int i = 0; i < n; i++) {
			sum += y[i];
		}
		return sum / n;
	}
	
	private double[] dist(int[] x) {
		double[] dist = new double[n];
		for (int i = 0; i < dist.length; i++) {
			dist[i] = dists.get(distType).apply(f[i], x);
		}
		return dist;
	}
	
	private double weight(int i, double[] distQ, double[] distQSorted) {
		double dist = distQ[i];
		double den;
		if (windowType.equals("fixed")) {
			den = h;
		} else if (windowType.equals("variable")) {
			den = distQSorted[k]; // k + 1 nearest
		} else {
			throw new AssertionError("Invalid window type");
		}
		return kernels.get(kernelType).applyAsDouble(dist / den);
	}
	
	// dists
	private double manhattan(int[] x1, int[] x2) {
		return minkowski(x1, x2, 1);
	}
	
	private double euclidean(int[] x1, int[] x2) {
		return minkowski(x1, x2, 2);
	}
	
	private double chebyshev(int[] x1, int[] x2) {
		double max = 0;
		for (int i = 0; i < m; i++) {
			max = Math.max(max, Math.abs(x1[i] - x2[i]));
		}
		return max;
	}
	
	private double minkowski(int[] x1, int[] x2, int p) {
		double sum = 0;
		for (int i = 0; i < m; i++) {
			sum += Math.pow(Math.abs(x1[i] - x2[i]), p);
		}
		return Math.pow(sum, 1.0 / p);
	}
	
	// kernels
	private double uniform(double u) {
		if (Math.abs(u) >= 1) {
			return 0;
		}
		return 0.5;
	}
	
	private double triangular(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 1 - Math.abs(u);
	}
	
	private double epanechnikov(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 3.0 / 4.0 * (1 - u * u);
	}
	
	private double quartic(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 15.0 / 16.0 * Math.pow(1 - u * u, 2);
	}
	
	private double triweight(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 35.0 / 32.0 * Math.pow(1 - u * u, 3);
	}
	
	private double tricube(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return 70.0 / 81.0 * Math.pow(1 - Math.pow(Math.abs(u), 3), 3);
	}
	
	private double gaussian(double u) {
		return 1 / Math.sqrt(2 * Math.PI) * Math.exp(-0.5 * u * u);
	}
	
	private double cosine(double u) {
		if (Math.abs(u) > 1) {
			return 0;
		}
		return Math.PI / 4 * Math.cos(Math.PI / 2 * u);
	}
	
	private double logistic(double u) {
		return 1 / (Math.exp(u) + 2 + Math.exp(-u));
	}
	
	private double sigmoid(double u) {
		return 2 / Math.PI * 1 / (Math.exp(u) + Math.exp(-u));
	}
}
