import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;

public class LinearRegression {
	
	String FILE_NAME = "dataset/6.txt";
	int n, n_test, m;
	double[][] f, f_test;
	double[] y, y_test;
	
	public static void main(String[] args) throws IOException, PythonExecutionException, InterruptedException, ExecutionException {
		new LinearRegression().run();
	}
	
	private void readInput() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(FILE_NAME)));
		m = Integer.parseInt(in.readLine()) + 1;
		n = Integer.parseInt(in.readLine());
		f = new double[n][m];
		y = new double[n];
		readDataset(in, f, y);
		
		n_test = Integer.parseInt(in.readLine());
		f_test = new double[n_test][m];
		y_test = new double[n_test];
		readDataset(in, f_test, y_test);
		in.close();
	}
	
	private void readDataset(BufferedReader in, double[][] f, double[] y) throws IOException {
		String[] line;
		for (int i = 0; i < f.length; i++) {
			line = in.readLine().trim().split(" ");
			for (int j = 0; j < f[0].length; j++) {
				if (j < m - 1) {
					f[i][j] = Double.parseDouble(line[j]);
				} else {
					f[i][j] = 1;
					y[i] = Integer.parseInt(line[j]);
				}
			}
		}
	}
	
	private void run() throws IOException, PythonExecutionException, InterruptedException, ExecutionException {
		readInput();
		normalize();

		double[] w = stochastic(); // error = 0.009916484002114975
//		double[] w = svd(); // error = 0.005888335876127421
//		double[] w = random(); // error = 0.044488550788255414
		
		System.out.println(Arrays.toString(w));
		System.out.println("Error: " + error(f_test, y_test, w));
		plot(w);
	}
	
	private double[] stochastic() throws InterruptedException, IOException, PythonExecutionException, ExecutionException {
		return stochastic(new GradientDescent(), 1e-2, 0.21, (int) 1e6); // error = 0.009916484002114975
//		return stochasticStress();
	}
	
	private double[] stochastic(GradientDescent gs, double h, double tau, int iterations) throws IOException, PythonExecutionException {
		return gs.stochastic(f, y, h, tau, iterations, f_test, y_test);
	}
	
	private double[] stochasticStress() throws IOException, PythonExecutionException, InterruptedException, ExecutionException {
		double[] taus = new double[10];
		for (int i = 0; i < taus.length; i++) {
			taus[i] = (1.0 * taus.length / 2 - i) / 25 + 0.25;
		}
		System.out.println(Arrays.toString(taus));
		ExecutorService executor = Executors.newFixedThreadPool(taus.length);
		
		GradientDescent gd = new GradientDescent();
		Map<Double, Future<double[]>> tau2w = new HashMap<>();
		for (double tau : taus) {
			tau2w.put(tau, executor.submit(() -> stochastic(gd, 1e-2, tau, (int) 1e5)));
		}
		executor.shutdown();
		executor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
		
		Map<Double, Double> tau2error = new TreeMap<>();
		double[] w = new double[m];
		double error = Integer.MAX_VALUE;
		double tau = 0;
		for (var tauW : tau2w.entrySet()) {
			double curTau = tauW.getKey();
			double[] curW = tauW.getValue().get();
			
			double curE = error(f_test, y_test, curW);
			tau2error.put(curTau, curE);
			if (error > curE) {
				error = curE;
				w = curW;
				tau = curTau;
			}
		}
		System.out.printf("h: %f, tau = %f, nrmse: %s%n", 1e-2, tau, error);
		Plotter.linePlot("tau = " + tau, "tau", "nrmse",
				new ArrayList<>(tau2error.keySet()), new ArrayList<>(tau2error.values()));
		return w;
	}
	
	private double[] svd() throws IOException, PythonExecutionException {
//		return svd(new LeastSquares(), 2.3e-2); // error = 0.005888335876127421
		return svdStress();
	}
	
	private double[] svd(LeastSquares ls, double tau) {
		return ls.singularValueDecomposition(f, y, tau);
	}
	
	private double[] svdStress() throws IOException, PythonExecutionException {
		LeastSquares ls = new LeastSquares();
		Map<Double, Double> test = new TreeMap<>();
		Map<Double, Double> train = new TreeMap<>();
		double[] w = new double[m];
		double error = Integer.MAX_VALUE;
		double start = 0;
		double end = 3e-2;
		double step = (end - start) / 10;
		double tau = start;
		for (double curTau = start; curTau < end; curTau += step) {
			double[] curW = svd(ls, curTau);
			double curError = error(f_test, y_test, curW);
			test.put(1.0 * curTau, curError);
			train.put(1.0 * curTau, error(f, y, curW));
			if (error > curError) {
				error = curError;
				w = curW;
				tau = curTau;
			}
			System.out.println((int) (100 * (curTau - start) / (end - start)) + "% completed");
		}
		System.out.println("tau: " + tau);
		Plotter.linePlot("tau = " + tau, "tau", "nrmse",
				new ArrayList<>(test.keySet()), new ArrayList<>(test.values()));
		Plotter.linePlot("tau = " + tau, "tau", "nrmse",
				new ArrayList<>(train.keySet()), new ArrayList<>(train.values()));
		return w;
	}
	
	private double[] randomStress() throws IOException, PythonExecutionException, InterruptedException, ExecutionException {
		double[] taus = new double[10];
		for (int i = 0; i < taus.length; i++) {
			taus[i] = (1.0 * taus.length / 2 - i) / 30 + 0.7;
		}
		System.out.println(Arrays.toString(taus));
		ExecutorService executor = Executors.newFixedThreadPool(taus.length);
		
		RandomLR r = new RandomLR();
		Map<Double, Future<double[]>> tau2w = new HashMap<>();
		for (double tau : taus) {
			tau2w.put(tau, executor.submit(() -> random(r, tau, (int) 1e4)));
		}
		executor.shutdown();
		executor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
		
		Map<Double, Double> tau2error = new TreeMap<>();
		double[] w = new double[m];
		double error = Integer.MAX_VALUE;
		double tau = 0;
		for (var tauW : tau2w.entrySet()) {
			double curTau = tauW.getKey();
			double[] curW = tauW.getValue().get();
			
			double curE = error(f_test, y_test, curW);
			tau2error.put(curTau, curE);
			if (error > curE) {
				error = curE;
				w = curW;
				tau = curTau;
			}
		}
		System.out.printf("h: %f, tau = %f, nrmse: %s%n", 1e-2, tau, error);
		Plotter.linePlot("tau = " + tau, "tau", "nrmse",
				new ArrayList<>(tau2error.keySet()), new ArrayList<>(tau2error.values()));
		return w;
	}
	
	private double[] random(RandomLR r, double tau, int iterations) throws IOException, PythonExecutionException {
		return r.linearRegression(f, y, tau, iterations, f_test, y_test);
	}
	
	private double[] random() throws InterruptedException, PythonExecutionException, ExecutionException, IOException {
		return new RandomLR().linearRegression(f, y, 0.63, (int) 1e5, f_test, y_test); // error = 0.044488550788255414
//		return randomStress();
	}
	
	private void plot(double[] w) throws IOException, PythonExecutionException {
		if (m == 2) {
			List<Double> xs = new ArrayList<>();
			List<Double> ys = new ArrayList<>();
			for (int i = 0; i < n; i++) {
				xs.add(f[i][0]);
				ys.add(y[i]);
			}
			Plotter.scatterPlot("x", "y", xs, ys, w);
		}
	}
	
	private double error(double[][] f, double[] y, double[] w) {
		double[] pred = new double[y.length];
		for (int i = 0; i < f.length; i++) {
			pred[i] = scalar(f[i], w);
		}
		return Distance.nrmse(pred, y);
	}
	
	private void normalize() {
		normalize(f);
		normalize(f_test);
		normalize(y);
		normalize(y_test);
	}
	
	private void normalize(double[][] a) {
		for (int j = 0; j < a[0].length; j++) {
			double min = a[0][j];
			double max = a[0][j];
			for (double[] row : a) {
				min = Math.min(min, row[j]);
				max = Math.max(max, row[j]);
			}
			for (int i = 0; i < a.length; i++) {
				if (max - min == 0) {
					a[i][j] = 0.5;
				} else {
					a[i][j] = (a[i][j] - min) / (max - min);
				}
			}
		}
	}
	
	private void normalize(double[] a) {
		double min = a[0];
		double max = a[0];
		for (double v : a) {
			min = Math.min(min, v);
			max = Math.max(max, v);
		}
		for (int i = 0; i < a.length; i++) {
			if (max - min == 0) {
				a[i] = 0.5;
			} else {
				a[i] = (a[i] - min) / (max - min);
			}
		}
	}
	
	private double scalar(double[] a, double[] b) {
		double res = 0;
		for (int i = 0; i < a.length; i++) {
			res += a[i] * b[i];
		}
		return res;
	}
	
}
