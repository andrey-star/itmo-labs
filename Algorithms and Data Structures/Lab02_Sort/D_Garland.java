import java.io.*;
import java.util.Arrays;

public class D_Garland {
	
	private static double b;
	
	private static double lowest(int n, double a, double sec) {
		double[] res = new double[n];
		res[0] = a;
		res[1] = sec;
		double min = a;
		for (int i = 2; i < n; i++) {
			res[i] = 2 * res[i - 1] - res[i - 2] + 2;
			min = Math.min(min, res[i]);
		}
		b = res[n - 1];
		return min;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("garland.in")));
		String[] line = in.readLine().trim().split(" +");
		in.close();
		int n = Integer.parseInt(line[0]);
		double a = Double.parseDouble(line[1]);
		double minSec = 0;
		double maxSec = a;
		while (maxSec - minSec > 0.00001) {
			double mid = (minSec + maxSec) / 2;
			if (lowest(n, a, mid) > 0) {
				maxSec = mid;
			} else {
				minSec = mid;
			}
		}
		PrintWriter out = new PrintWriter(new File("garland.out"));
		out.println(String.format("%.2f", b).replace(",", "."));
		out.close();
	}
	
}
