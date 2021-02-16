import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.IOException;
import java.util.*;

public class GradientDescent {
	
	public double[] stochastic(double[][] xs, double[] ys, double h, double tau, int iters, double[][] f_test, double[] y_test) throws IOException, PythonExecutionException {
		Random RANDOM = new Random(4);
		int m = xs[0].length;
		double[] w = initW(xs.length, m);
//		double[] q = new double[m];
//		for (int i = 0; i < n; i++) {
//			double[] alpha_i = rmse(xs[i], w, ys[i]);
//			for (int j = 0; j < m; j++) {
//				q[j] += alpha_i[j] / n;
//			}
//		}
		TreeMap<Double, Double> test = new TreeMap<>();
		TreeMap<Double, Double> train = new TreeMap<>();
		for (int iter = 1; iter < iters; iter++) {
			int index = RANDOM.nextInt(xs.length);
			double[] x = xs[index];
			double y = ys[index];
			double[] grad = dMse(x, w, y);
			for (int j = 0; j < m; j++) {
				w[j] = w[j] * (1 - h / iter * tau) - h / iter * grad[j];
			}
			double[] pred_test = new double[y_test.length];
			for (int i = 0; i < f_test.length; i++) {
				pred_test[i] = scalar(f_test[i], w);
			}
			double[] pred_train = new double[ys.length];
			for (int i = 0; i < xs.length; i++) {
				pred_train[i] = scalar(xs[i], w);
			}
			if (iter % 100 == 0) {
				train.put(1.0 * iter, Distance.nrmse(pred_train, ys));
				test.put(1.0 * iter, Distance.nrmse(pred_test, y_test));
			}
			if (iter % 10000 == 0) {
				System.out.println((int) (100.0 * iter / iters) + "% completed");
			}
//			double[] epsilon_i = mse(x, w, y);
//			for (int j = 0; j < m; j++) {
//				q[j] = lambda * epsilon_i[j] + (1 - lambda) * q[j];
//			}
		}
		Plotter.linePlot("h = " + h + ", tau = " + tau + ", error = " + test.lastEntry().getValue(),
				"iteration", "nrmse", new ArrayList<>(test.keySet()), new ArrayList<>(test.values()));
		Plotter.linePlot("h = " + h + ", tau = " + tau + ", error = " + train.lastEntry().getValue(),
				"iteration", "nrmse", new ArrayList<>(train.keySet()), new ArrayList<>(train.values()));
		return w;
	}
	
	private double[] dMse(double[] x, double[] w, double y) {
		double xwScalar = scalar(x, w);
		double[] res = new double[w.length];
		for (int i = 0; i < x.length; i++) {
			res[i] = (xwScalar - y) * x[i];
		}
		return res;
	}
	
	/*
	private double[] rmse(double[] x, double[] w, double y) {
		double xwScalar = scalar(x, w);
		double[] res = new double[w.length];
		for (int i = 0; i < x.length; i++) {
			res[i] = Math.abs(xwScalar - y);
		}
		return res;
	}
	
	private double[] smape(double[] x, double[] w, double y) {
		double xwScalar = scalar(x, w);
		double[] res = new double[w.length];
		for (int i = 0; i < x.length; i++) {
			double nom = Math.abs(xwScalar - y);
			double den = Math.abs(xwScalar) + Math.abs(y);
			res[i] = den == 0 ? 0 : nom / den;
		}
		return res;
	}
	
	private double[] dSmape(double[] x, double[] w, double y) {
		double xwScalar = scalar(x, w);
		double[] res = new double[w.length];
		for (int i = 0; i < x.length; i++) {
			double a = Math.signum(xwScalar - y) * x[i] * (Math.abs(xwScalar) + Math.abs(y));
			double b = Math.abs(xwScalar - y) * Math.signum(xwScalar) * x[i];
			double den = Math.pow(Math.abs(xwScalar) + Math.abs(y), 2);
			res[i] = den == 0 ? 0 : (a - b) / den;
			System.out.printf("xw: %f, y; %s, a - b: %f, den: %f, res: %f%n", xwScalar, y, a - b, den, res[i]);
		}
		return res;
	}
	
	private double[] sigmoid(double[] x, double[] w, double y) {
		double xwScalar = scalar(x, w);
		double[] res = new double[w.length];
		double margin = xwScalar * y;
		for (int i = 0; i < x.length; i++) {
			double sigm = 2 / (1 + Math.exp(margin));
			res[i] = sigm;
		}
		return res;
	}
	
	private double[] dSigmoid(double[] x, double[] w, double y) {
		double xwScalar = scalar(x, w);
		double margin = xwScalar * y;
		double eProd = Math.exp(margin);
		double base = -2 * eProd * y / Math.pow(1 + eProd, 2);
		double[] res = new double[w.length];
		for (int i = 0; i < x.length; i++) {
			res[i] = x[i] * base;
		}
		return res;
	}
	*/
	
	private double[] initW(int n, int m) {
		Random random = new Random(5);
		double[] doubles = new double[m];
		for (int i = 0; i < m; i++) {
			doubles[i] = (random.nextDouble() - 0.5) / (2 * n);
		}
		return doubles;
	}
	
	private double scalar(double[] a, double[] b) {
		double res = 0;
		for (int i = 0; i < a.length; i++) {
			res += a[i] * b[i];
		}
		return res;
	}
	
}
