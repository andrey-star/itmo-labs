import java.io.*;
import java.util.Arrays;

public class D_Markchain {
	
	private static double[][] square(double[][] a) {
		int n = a.length;
		double[][] res = new double[n][n];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				for (int k = 0; k < n; k++) {
					res[i][j] += a[i][k] * a[k][j];
				}
			}
		}
		return res;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("markchain.in")));
		int n = Integer.parseInt(in.readLine());
		String[] line;
		double[][] mat = new double[n][n];
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			for (int j = 0; j < n; j++) {
				mat[i][j] = Double.parseDouble(line[j]);
			}
		}
		for (int i = 0; i < 20; i++) {
			mat = square(mat);
		}
		PrintWriter out = new PrintWriter(new File("markchain.out"));
		double sum = Arrays.stream(mat[0]).reduce(0, Double::sum);
		for (int i = 0; i < n; i++) {
			out.println(mat[0][i] / sum);
		}
		out.close();
	}
	
}
