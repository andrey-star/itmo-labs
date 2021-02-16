import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @see <a href="http://cs229.stanford.edu/materials/smo.pdf">http://cs229.stanford.edu/materials/smo.pdf</a>
 */
public class SimplifiedSMO {
	
	private static final double EPS = 1e-5;
	private static final int CV_CHUNKS = 4;
	private static final Random RANDOM = new Random(4);
	
	private final int n;
	private final double[][] xs;
	private final double[] ys;
	private final BiFunction<double[], double[], Double> kernel;
	
	public SimplifiedSMO(double[][] xs, double[] ys, BiFunction<double[], double[], Double> kernel) {
		this.n = ys.length;
		this.xs = xs;
		this.ys = ys;
		this.kernel = kernel;
	}
	
	public Model crossValidation(double c, double iters) {
		List<Integer> order = IntStream.range(0, n).boxed().collect(Collectors.toList());
		Collections.shuffle(order);
		double totalAccuracy = 0;
		for (int chunk = 0; chunk < CV_CHUNKS; chunk++) {
			int start = n / CV_CHUNKS * chunk;
			int end = n / CV_CHUNKS * (chunk + 1);
			if (chunk == CV_CHUNKS - 1) {
				end = n;
			}
			double[][] x_test = new double[end - start][xs[0].length];
			double[] y_test = new double[end - start];
			for (int i = start; i < end; i++) {
				x_test[i - start] = xs[order.get(i)];
				y_test[i - start] = ys[order.get(i)];
			}
			double[][] x_train = new double[n - x_test.length][xs[0].length];
			double[] y_train = new double[n - y_test.length];
			boolean passedChunk = false;
			for (int i = 0; i < n; i++) {
				if (start <= i && i < end) {
					passedChunk = true;
					continue;
				}
				int newIndex = passedChunk ? i - (end - start) : i;
				x_train[newIndex] = xs[order.get(i)];
				y_train[newIndex] = ys[order.get(i)];
			}
			Model model = minimize(x_train, y_train, c, iters);
			double accuracy = getAccuracy(x_test, y_test, model);
			totalAccuracy += accuracy;
		}
		Model model = minimize(xs, ys, c, iters);
		model.cvAccuracy = totalAccuracy / CV_CHUNKS;
		model.accuracy = getAccuracy(xs, ys, model);
		return model;
	}
	
	public double getAccuracy(double[][] xs, double[] ys, Model model) {
		int p = 0;
		int n = 0;
		int tp = 0;
		int tn = 0;
		for (int i = 0; i < xs.length; i++) {
			double expected = ys[i];
			double actual = pred(xs, ys, i, model.lambda, model.b);
			if (Math.signum(expected) > 0) {
				p++;
				if (Math.signum(actual) > 0) {
					tp++;
				}
			} else if (Math.signum(expected) < 0) {
				n++;
				if (Math.signum(actual) < 0) {
					tn++;
				}
			}
//			System.out.println(Arrays.toString(xs[i]) + ": expected " + expected + ", got " + actual);
		}
		return 1.0 * (tp + tn) / (p + n);
	}
	
	public Model minimize(double[][] xs, double[] ys, double c, double iters) {
		int n = ys.length;
		double[] alpha = new double[n];
		double b = 0;
		for (int iter = 0; iter < iters; iter++) {
			List<Integer> ind = IntStream.range(0, n).boxed().collect(Collectors.toList());
			Collections.shuffle(ind, RANDOM);
			for (int i = 0; i < n; i++) {
				double e_i = pred(xs, ys, i, alpha, b) - ys[i];
				if ((ys[i] * e_i < -EPS && alpha[i] < c) || (ys[i] * e_i > EPS && alpha[i] > 0)) {
					int j = ind.get(i);
					if (j == i) {
						continue;
					}
					double e_j = pred(xs, ys, j, alpha, b) - ys[j];
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
					double eta = 2 * kernel.apply(xs[i], xs[j]) - kernel.apply(xs[i], xs[i]) - kernel.apply(xs[j], xs[j]);
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
							- ys[i] * (alpha_i_new - alpha[i]) * kernel.apply(xs[i], xs[i])
							- ys[j] * (alpha_j_new - alpha[j]) * kernel.apply(xs[i], xs[j]);
					double b2 = b - e_j
							- ys[i] * (alpha_i_new - alpha[i]) * kernel.apply(xs[i], xs[j])
							- ys[j] * (alpha_j_new - alpha[j]) * kernel.apply(xs[j], xs[j]);
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
		return new Model(alpha, b);
	}
	
	protected double pred(double[][] xs, double[] ys, int i, double[] lambda, double b) {
		double res = 0;
		for (int j = 0; j < xs.length; j++) {
			res += lambda[j] * ys[j] * kernel.apply(xs[i], xs[j]);
		}
		return res + b;
	}
	
	public static class Model {
		double[] lambda;
		double b;
		double accuracy;
		double cvAccuracy;
		
		public Model(double[] lambda, double b) {
			this.lambda = lambda;
			this.b = b;
		}
	}
	
}
