import java.io.*;
import java.util.Arrays;

public class C_Floyd {
	
	private static final long INF = Long.MAX_VALUE / 2;
	
	private static void solve(long[][] d) {
		int n = d.length;
		for (int k = 0; k < n; k++) {
			for (int u = 0; u < n; u++) {
				for (int v = 0; v < n; v++) {
					d[u][v] = Math.min(d[u][v], d[u][k] + d[k][v]);
				}
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		long[][] d = new long[n][n];
		for (long[] longs : d) {
			Arrays.fill(longs, INF);
		}
		for (int i = 0; i < n; i++) {
			d[i][i] = 0;
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int w = Integer.parseInt(line[2]);
			d[a][b] = w;
		}
		solve(d);
		PrintWriter out = new PrintWriter(System.out);
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				out.print(d[i][j] + " ");
			}
			out.println();
		}
		out.close();
	}
	
}