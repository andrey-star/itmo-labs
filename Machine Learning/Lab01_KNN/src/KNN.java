import java.io.*;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class KNN {
	
	int n, m;
	double[][] f;
	int[] y;
	Map<String, Integer> classes;

	public static void main(String[] args) throws IOException {
		new KNN().run();
	}
	
	private void readData() throws IOException {
//		String fileName = "dataset/dataset_61_iris.csv";
		String fileName = "dataset/ecoli.csv";
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));
		System.out.println("Reading " + fileName + "...");
		String[][] dataset = in.lines().map(line -> line.split(",")).toArray(String[][]::new);
		in.close();
		
		classes = new HashMap<>();
		n = dataset.length - 1;
		m = dataset[0].length - 1;
		f = new double[n][m];
		y = new int[n];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < m + 1; j++) {
				String value = dataset[i + 1][j];
				if (j < m) {
					f[i][j] = Double.parseDouble(value);
				} else {
					classes.putIfAbsent(value, classes.size());
					y[i] = classes.get(value);
				}
			}
		}
	}
	
	private void run() throws IOException {
		readData();
		normalize();
//		Run bestRun = stress();
		Run bestRun = new Run(0.885443, "epanechnikov", "euclidean", "variable", -1, 13);
//		Run bestFixedRun = new Run(0.882651, "logistic", "euclidean", "variable", 0.04, -1);
		System.out.println((bestRun.windowType.equals("fixed") ? "h" : "k") + ",f1");
		for (double h = 0; h <= 1; h += 0.01) {
			int k = (int) (h * (f.length - 2));
			Run run = calculateWeightedF1(bestRun.kernelType, bestRun.distType, bestRun.windowType, h, k);
			System.out.printf(Locale.US, "%f,%f%n", bestRun.windowType.equals("fixed") ? h : k, run.f1);
		}
	}
	
	private void sample(String kernelType, String distType, String windowType, double h, int k) {
		System.out.println(calculateWeightedF1(kernelType, distType, windowType, h, k));
	}
	
	private Run stress() {
		String[] windows = {"fixed", "variable"};
		int combs = Kernel.kernels.size() * Distance.dists.size() * windows.length;
		int granularity = 50;
		
		long start = System.currentTimeMillis();
		Run bestRun = new Run();
		int curRun = 0;
		for (String kernelType : Kernel.kernels.keySet()) {
			for (String distType : Distance.dists.keySet()) {
				for (String windowType : windows) {
					curRun++;
					System.out.printf("%d/%d: %s %s %s\n", curRun, combs, kernelType, distType, windowType);
					for (double h = 0; h <= 1; h += 1.0 / granularity) {
						int k = (int) (h * (f.length - 2));
						Run run = calculateWeightedF1(kernelType, distType, windowType, h, k);
						if (bestRun.compareTo(run) < 0) {
							bestRun = run;
						}
					}
				}
			}
		}
		System.out.println("\nResult:\n" + bestRun);
		System.out.println("Granularity: " + granularity);
		System.out.printf("%nFinished in: %sms%n", (System.currentTimeMillis() - start) / 1000.0);
		return bestRun;
	}
	
	private Run calculateWeightedF1(String kernelType, String distType, String windowType, double h, int k) {
		int classNum = classes.size();
		var npr = new NonParamRegression(n - 1, m, classNum, kernelType, distType, windowType, h, k);
		int[][] confusion = new int[classNum][classNum];
		for (int i = 0; i < n; i++) {
			double[] testX = f[i];
			int actualY = y[i];
			
			double[][] trainF = leaveOneOut(i, f);
			int[] trainY = leaveOneOut(i, y);
			
			int predictedY = npr.onehot(testX, trainF, trainY);
			confusion[predictedY][actualY]++;
		}
		
		return new Run(npr, getWeightedF1(confusion));
	}
	
	private double getWeightedF1(int[][] confusion) {
		double weightedf1 = 0;
		for (int i = 0; i < confusion.length; i++) {
			double tpfp = 0;
			double p = 0;
			for (int j = 0; j < confusion[0].length; j++) {
				tpfp += confusion[i][j];
				p += confusion[j][i];
			}
			int tp = confusion[i][i];
			double precision = tpfp == 0 ? 0 : tp / tpfp;
			double recall = p == 0 ? 0 : tp / p;
			double f1 = precision + recall == 0 ? 0 : 2 * precision * recall / (precision + recall);
			weightedf1 += f1 * p;
		}
		weightedf1 /= f.length;
		return weightedf1;
	}
	
	private double[][] leaveOneOut(int toLeave, double[][] a) {
		double[][] loo = new double[a.length - 1][a[0].length];
		for (int i = 0; i < a.length; i++) {
			for (int j = 0; j < a[0].length; j++) {
				if (i < toLeave) {
					loo[i][j] = a[i][j];
				} else if (i > toLeave) {
					loo[i - 1][j] = a[i][j];
				}
			}
		}
		return loo;
	}
	
	private int[] leaveOneOut(int toLeave, int[] a) {
		int[] loo = new int[a.length - 1];
		for (int i = 0; i < a.length; i++) {
			if (i < toLeave) {
				loo[i] = a[i];
			} else if (i > toLeave) {
				loo[i - 1] = a[i];
			}
		}
		return loo;
	}
	
	private void normalize() {
		for (int j = 0; j < f[0].length; j++) {
			double min = f[0][j];
			double max = f[0][j];
			for (double[] row : f) {
				min = Math.min(min, row[j]);
				max = Math.max(max, row[j]);
			}
			for (int i = 0; i < f.length; i++) {
				f[i][j] = (f[i][j] - min) / (max - min);
			}
		}
	}
	
	private static class Run implements Comparable<Run> {
		final double f1;
		final String kernelType, distType, windowType;
		final double h;
		final int k;
		
		public Run(NonParamRegression npr, double f1) {
			this.f1 = f1;
			this.kernelType = npr.kernelType;
			this.distType = npr.distType;
			this.windowType = npr.windowType;
			this.h = npr.h;
			this.k = npr.k;
		}
		
		public Run(double f1, String kernelType, String distType, String windowType, double h, int k) {
			this.f1 = f1;
			this.kernelType = kernelType;
			this.distType = distType;
			this.windowType = windowType;
			this.h = h;
			this.k = k;
		}
		
		public Run() {
			f1 = 0;
			kernelType = "";
			distType = "";
			windowType = "";
			h = 0;
			k = 0;
		}
		
		@Override
		public int compareTo(Run o) {
			return Double.compare(f1, o.f1);
		}
		
		@Override
		public String toString() {
			return String.format("f1 = %f\nKernel: %s\nDist: %s\nWindow: %s\n"
							+ (windowType.equals("fixed") ? "h = %f" : "k = %f"),
					f1, kernelType, distType, windowType, windowType.equals("fixed") ? h : k);
		}
	}
}
