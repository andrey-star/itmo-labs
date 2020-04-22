import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class G_Prim {
	
	private static final double INF = Double.MAX_VALUE;
	
	private static double solve(int[] xs, int[] ys) {
		int n = xs.length;
		boolean[] used = new boolean[n];
		double[] min = new double[n];
		Arrays.fill(min, INF);
		min[0] = 0;
		double res = 0;
		for (int i = 0; i < n; i++) {
			int u = -1;
			for (int j = 0; j < n; j++) {
				if (!used[j] && (u == -1 || min[j] < min[u])) {
					u = j;
				}
			}
			used[u] = true;
			res += min[u];
			for (int v = 0; v < n; v++) {
				int dx = Math.abs(xs[u] - xs[v]);
				int dy = Math.abs(ys[u] - ys[v]);
				double val = Math.sqrt(dx * dx + dy * dy);
				min[v] = Math.min(min[v], val);
			}
		}
		return res;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int[] xs = new int[n];
		int[] ys = new int[n];
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int x = Integer.parseInt(line[0]);
			int y = Integer.parseInt(line[1]);
			xs[i] = x;
			ys[i] = y;
		}
		System.out.println(solve(xs, ys));
	}
	
}