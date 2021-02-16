import java.util.Arrays;

public class NonParamRegression {
	
	int n, m;
	double[][] f;
	int[] y; // 0, 1, 2, ...
	String distType, kernelType, windowType;
	double h;
	int k;
	int classes;
	
	public NonParamRegression(int n, int m, int classes, String kernelType, String distType, String windowType, double h, int k) {
		this.n = n;
		this.m = m;
		this.classes = classes;
		this.kernelType = kernelType;
		this.distType = distType;
		this.windowType = windowType;
		this.h = h;
		this.k = k;
	}
	
	public int naive(double[] q, double[][] f, int[] y) {
		this.f = f;
		this.y = y;
		return (int) Math.round(solve(q));
	}
	
	public int onehot(double[] q, double[][] f, int[] y) {
		double[] res = new double[classes];
		double max = 0;
		int maxClass = 0;
		this.f = f;
		this.y = new int[y.length];
		for (int clas = 0; clas < classes; clas++) {
			for (int i = 0; i < y.length; i++) {
				this.y[i] = y[i] == clas ? 1 : 0;
			}
			res[clas] = solve(q);
			if (max < res[clas]) {
				max = res[clas];
				maxClass = clas;
			}
		}
		return maxClass;
	}
	
	private double solve(double[] q) {
		double[] distQ = dist(q);
		double[] distQSorted = Arrays.copyOf(distQ, n);
		Arrays.sort(distQSorted);
		if ((windowType.equals("fixed") && h == 0) || (windowType.equals("variable") && distQSorted[k] == 0)) {
			return averageY(distQ);
		}
		double nom = 0;
		double den = 0;
		for (int i = 0; i < n; i++) {
			double yi = y[i];
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
		// between same points
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
		// between all points
		sum = 0;
		for (int i = 0; i < n; i++) {
			sum += y[i];
		}
		return sum / n;
	}
	
	private double[] dist(double[] x) {
		double[] dist = new double[n];
		for (int i = 0; i < dist.length; i++) {
			dist[i] = Distance.dists.get(distType).apply(f[i], x);
		}
		return dist;
	}
	
	private double weight(int i, double[] distQ, double[] distQSorted) {
		double dist = distQ[i];
		double den;
		if (windowType.equals("fixed")) {
			den = h;
		} else if (windowType.equals("variable")) {
			if (k >= distQSorted.length) {
				throw new AssertionError("no kth member present");
			}
			den = distQSorted[k]; // k + 1 nearest
		} else {
			throw new AssertionError("Invalid window type");
		}
		return Kernel.kernels.get(kernelType).applyAsDouble(dist / den);
	}
	
}