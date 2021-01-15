import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @see <a href="http://cs229.stanford.edu/materials/smo.pdf">http://cs229.stanford.edu/materials/smo.pdf</a>
 */
public class E_SimplifiedSMO {
	
	private static final double EPS = 1e-5;
	private static final Random RANDOM = new Random(4);
	
	private final int m;
	private final double[][] xs;
	private final double[] ys;
	private final BiFunction<Integer, Integer, Double> kernel;
	
	public E_SimplifiedSMO(double[][] xs, double[] ys, BiFunction<Integer, Integer, Double> kernel) {
		this.m = ys.length;
		this.xs = xs;
		this.ys = ys;
		this.kernel = kernel;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		double[][] xs = new double[n][n];
		double[] ys = new double[n];
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			for (int j = 0; j < n + 1; j++) {
				double val = Integer.parseInt(line[j]);
				if (j < n) {
					xs[i][j] = val;
				} else {
					ys[i] = val;
				}
			}
		}
		double c = Integer.parseInt(in.readLine());
		in.close();
		
		Pair minimize = new E_SimplifiedSMO(xs, ys, (i, j) -> xs[i][j]).minimize(c, (int) 1e3);
		for (double v : minimize.a) {
			System.out.printf(Locale.US, "%.8f%n", v);
		}
		System.out.printf(Locale.US, "%.8f%n", minimize.b);
	}
	
	public Pair minimize(double c, double iters) {
		double[] alpha = new double[m];
		double b = 0;
		for (int iter = 0; iter < iters; iter++) {
			List<Integer> ind = IntStream.range(0, m).boxed().collect(Collectors.toList());
			Collections.shuffle(ind, RANDOM);
			for (int i = 0; i < m; i++) {
				double e_i = pred(i, alpha, b) - ys[i];
				if ((ys[i] * e_i < -EPS && alpha[i] < c) || (ys[i] * e_i > EPS && alpha[i] > 0)) {
					int j = ind.get(i);
					if (j == i) {
						continue;
					}
					double e_j = pred(j, alpha, b) - ys[j];
					double l, h;
					if (ys[i] != ys[j]) {
						l = Math.max(0, alpha[j] - alpha[i]);
						h = Math.min(c, c + alpha[j] - alpha[i]);
					} else {
						l = Math.max(0, alpha[i] + alpha[j] - c);
						h = Math.min(c, alpha[i] + alpha[j]);
					}
					if (Math.abs(l - h) < EPS) {
						continue;
					}
					double eta = 2 * kernel.apply(i, j) - kernel.apply(i, i) - kernel.apply(j, j);
					if (eta >= 0) {
						continue;
					}
					double alpha_j_new = alpha[j] - ys[j] * (e_i - e_j) / eta;
					if (alpha_j_new > h) {
						alpha_j_new = h;
					} else if (alpha_j_new < l) {
						alpha_j_new = l;
					}
					if (Math.abs(alpha_j_new - alpha[j]) < EPS) {
						continue;
					}
					double alpha_i_new = alpha[i] + ys[i] * ys[j] * (alpha[j] - alpha_j_new);
					double b1 = b - e_i
							- ys[i] * (alpha_i_new - alpha[i]) * kernel.apply(i, i)
							- ys[j] * (alpha_j_new - alpha[j]) * kernel.apply(i, j);
					double b2 = b - e_j
							- ys[i] * (alpha_i_new - alpha[i]) * kernel.apply(i, j)
							- ys[j] * (alpha_j_new - alpha[j]) * kernel.apply(j, j);
					if (0 < alpha_i_new && alpha_i_new < c) {
						b = b1;
					} else if (0 < alpha_j_new && alpha_j_new < c) {
						b = b2;
					} else {
						b = (b1 + b2) / 2;
					}
					alpha[i] = alpha_i_new;
					alpha[j] = alpha_j_new;
				}
			}
		}
		return new Pair(alpha, b);
	}
	
	private double pred(int i, double[] alpha, double b) {
		double res = 0;
		for (int j = 0; j < m; j++) {
			res += alpha[j] * ys[j] * kernel.apply(i, j);
		}
		return res + b;
	}
	
	public static class Pair {
		double[] a;
		double b;
		
		public Pair(double[] a, double b) {
			this.a = a;
			this.b = b;
		}
	}
	
}
