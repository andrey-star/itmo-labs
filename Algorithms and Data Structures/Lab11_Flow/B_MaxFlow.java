import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class B_MaxFlow {
	
	private static int dfs(int u, int t, int cap, int[][] capacity, boolean[] used) {
		if (u == t) {
			return cap;
		}
		used[u] = true;
		int n = capacity.length;
		for (int v = 0; v < n; v++) {
			if (!used[v] && capacity[u][v] > 0) {
				int delta = dfs(v, t, Math.min(cap, capacity[u][v]), capacity, used);
				capacity[v][u] += delta;
				capacity[u][v] -= delta;
				if (delta > 0) {
					return delta;
				}
			}
		}
		return 0;
	}
	
	public static void main(String[] args) throws IOException {
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int[][] capacity = new int[n][n];
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int c = Integer.parseInt(line[2]);
			capacity[a][b] = c;
		}
		in.close();
		
		int maxFlow = 0;
		int delta;
		do {
			delta = dfs(0, n - 1, Integer.MAX_VALUE, capacity, new boolean[n]);
			maxFlow += delta;
		} while (delta != 0);
		
		System.out.println(maxFlow);
	}
	
}
