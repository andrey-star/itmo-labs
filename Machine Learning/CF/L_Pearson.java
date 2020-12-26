import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Locale;

public class L_Pearson {
	
	public static void main(String[] args) throws IOException {
		new L_Pearson().run();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		double[] x = new double[n];
		double[] y = new double[n];
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			x[i] = Integer.parseInt(line[0]);
			y[i] = Integer.parseInt(line[1]);
		}
		double avX = average(x);
		double avY = average(y);
		double cov = 0;
		double dx = 0;
		double dy = 0;
		for (int i = 0; i < n; i++) {
			cov += (x[i] - avX) * (y[i] - avY);
			dx += (x[i] - avX) * (x[i] - avX);
			dy += (y[i] - avY) * (y[i] - avY);
		}
		double pearson;
		if (Math.abs(dx) < 1e-6 || Math.abs(dy) < 1e-6) {
			pearson = 0;
		} else {
			pearson = cov / Math.sqrt(dx * dy);
		}
		System.out.printf(Locale.US, "%.10f%n", pearson);
	}
	
	private double average(double[] a) {
		double sum = 0;
		for (double v : a) {
			sum += v;
		}
		return sum / a.length;
	}
}
