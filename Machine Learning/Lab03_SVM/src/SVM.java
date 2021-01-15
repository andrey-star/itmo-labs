import java.io.*;
import java.util.*;
import java.util.function.BiFunction;

public class SVM {
	
	private static final String FILE_NAME = "dataset/geyser.csv";
	private double[][] xs;
	private double[] ys;
	
	public static void main(String[] args) throws IOException {
		new SVM().run();
	}
	
	private void readData() throws IOException {
		System.out.println("Reading: " + FILE_NAME);
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(FILE_NAME)));
		int n = Integer.parseInt(in.readLine());
		int m = 2;
		xs = new double[n][m];
		ys = new double[n];
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(",");
			for (int j = 0; j < m + 1; j++) {
				if (j < m) {
					xs[i][j] = Double.parseDouble(line[j]);
				} else {
					ys[i] = line[j].equals("P") ? 1 : -1;
				}
			}
		}
		in.close();
	}
	
	private void run() throws IOException {
		readData();
		// geyser: C: 1, kernel: linear - 90%
		// geyser: C: 0.5, kernel: rbf(betta=2) - 91%
		// chips: C: 50.00, kernel: poly(d=5) - 86%
		stress();
//		SimplifiedSMO.Model smo = smo(new SimplifiedSMO(xs, ys, Kernel.rbf(2)), 0.5, (int) 5e3);
//		System.out.println(smo.accuracy);
//		System.out.println(smo.cvAccuracy);
//		System.out.println(Arrays.toString(smo.lambda));
//		System.out.println(smo.b);
	}
	
	private void stress() {
		Map<String, BiFunction<double[], double[], Double>> kernels = Map.of(
				"linear", Kernel::linear,
				"poly 2", Kernel.poly(2),
				"poly 3", Kernel.poly(3),
				"poly 4", Kernel.poly(4),
				"poly 5", Kernel.poly(5),
				"rbf 1", Kernel.rbf(1),
				"rbf 2", Kernel.rbf(2),
				"rbf 3", Kernel.rbf(3),
				"rbf 4", Kernel.rbf(4),
				"rbf 5", Kernel.rbf(5)
		);
		for (String kernelName : kernels.keySet()) {
			SimplifiedSMO smo = new SimplifiedSMO(xs, ys, kernels.get(kernelName));
			for (double c : new double[]{0.05, 0.1, 0.5, 1.0, 5.0, 10.0, 50.0, 100.0}) {
				SimplifiedSMO.Model model = smo(smo, c, (int) 1e3);
				System.out.printf(Locale.US, "C: %.2f, kernel: %s: %d%%%n", c, kernelName, (int) (model.accuracy * 100));
			}
		}
	}
	
	private SimplifiedSMO.Model smo(SimplifiedSMO smo, double c, int iters) {
		return smo.crossValidation(c, iters);
	}
	
}
