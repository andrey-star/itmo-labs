import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class RandomLR {
	
	double shift = 0.1;
	int m;
	double[][] f;
	double[] y;
	
	public double[] linearRegression(double[][] f, double[] y, double tau, int iters, double[][] f_test, double[] y_test) throws IOException, PythonExecutionException {
		Random random = new Random(4);
		m = f[0].length;
		this.f = f;
		this.y = y;
		double[] w = new double[m];
		for (int i = 0; i < m; i++) {
			w[i] = random.nextDouble();
		}
		double error = error(f, y, w, tau, true);
		double noChange = 0;
		List<Double> it = new ArrayList<>();
		List<Double> er = new ArrayList<>();
		for (int iter = 0; iter < iters; iter++) {
			if (noChange == 1000) {
				noChange = 0;
				shift /= 2;
			}
			int i = random.nextInt(m);
			double shift = random.nextBoolean() ? this.shift : -this.shift;
			w[i] += shift;
			double curError = error(f, y, w, tau, true);
			if (error > curError) {
				error = curError;
				noChange = 0;
			} else {
				w[i] -= shift;
				noChange++;
			}
			if (iter % 1000 == 0 && iter > 1000) {
				it.add(1.0 * iter);
				er.add(error(f_test, y_test, w, tau, false));
			}
			if (iter % 10000 == 0) {
				System.out.println((int) (100.0 * iter / iters) + "% completed");
			}
		}
		Plotter.linePlot("error = " + er.get(er.size() - 1),
				"iteration", "nrmse", it, er);
		return w;
	}
	
	private double error(double[][] f, double[] y, double[] w, double tau, boolean norm) {
		double[] pred = new double[y.length];
		for (int i = 0; i < f.length; i++) {
			pred[i] = scalar(f[i], w);
		}
		return Distance.nrmse(pred, y) + (norm ? tau * scalar(w, w) : 0);
	}
	
	private double scalar(double[] a, double[] b) {
		double res = 0;
		for (int i = 0; i < a.length; i++) {
			res += a[i] * b[i];
		}
		return res;
	}
	
}
