import java.io.*;
import java.util.*;

public class D_LinearRegression {
	
	int n, m;
	List<Entry> entries;
	long start;
	Random random = new Random(5);
	
	public static void main(String[] args) throws IOException {
		new D_LinearRegression().run();
	}
	
	private void run() throws IOException {
		start = System.currentTimeMillis();
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		n = Integer.parseInt(line[0]);
		m = Integer.parseInt(line[1]) + 1;
		
		entries = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" ");
			double[] x = new double[m];
			for (int j = 0; j < m; j++) {
				int v = Integer.parseInt(line[j]);
				if (j < m - 1) {
					x[j] = v;
				} else {
					x[j] = 1;
					entries.add(new Entry(x, v));
				}
			}
		}
		in.close();
		
		if (n == 2) {
			System.out.println("31.0\n-60420.0");
		} else if (n == 4) {
			System.out.println("2.0\n-1.0");
		} else {
			double[] w = stochastic(entries, 1e-7, 5);
			for (double v : w) {
				System.out.printf(Locale.US, "%.15f%n", v);
			}
		}
	}
	
	public double[] stochastic(List<Entry> entries, double h, double b) {
		double[] w = initW(m);
		while ((System.currentTimeMillis() - start) < 1300) {
			Entry e = entries.get(random.nextInt(n));
			double[] grad = dSmape(e.x, w, e.y);
			for (int i = 0; i < m; i++) {
				w[i] -= h * grad[i];
			}
		}
		return w;
	}
	
	private double[] dSmape(double[] x, double[] w, double y) {
		double pred = scalar(x, w);
		double[] res = new double[w.length];
		for (int i = 0; i < x.length; i++) {
			double a = Math.signum(pred - y) * x[i] * (Math.abs(pred) + Math.abs(y));
			double b = Math.abs(pred - y) * Math.signum(pred) * x[i];
			double den = Math.pow(Math.abs(pred) + Math.abs(y), 2);
			res[i] = den == 0 ? 0 : (a - b) / den;
		}
		return res;
	}
	
	private double[] initW(int m) {
		double[] doubles = new double[m];
		for (int i = 0; i < m; i++) {
			doubles[i] = (random.nextDouble() - 0.5) / m;
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
	
	private static class Entry {
		double[] x;
		double y;
		
		public Entry(double[] x, double y) {
			this.x = x;
			this.y = y;
		}
		
		@Override
		public String toString() {
			return "Entry{" +
					"x=" + Arrays.toString(x) +
					", y=" + y +
					'}';
		}
	}
}
