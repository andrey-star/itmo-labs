import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class A_Matching {
	
	private static boolean dfs(int u, List<Integer>[] g, boolean[] used, int[] a) {
		used[u] = true;
		for (int v : g[u]) {
			if (a[v] == -1) {
				a[v] = u;
				return true;
			}
			if (!used[a[v]] && dfs(a[v], g, used, a)) {
				a[v] = u;
				return true;
			}
		}
		return false;
	}
	
	public static void main(String[] args) throws IOException {
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]); // left
		int m = Integer.parseInt(line[1]); // right
		int k = Integer.parseInt(line[2]);
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < k; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
		}
		boolean[] used = new boolean[n];
		int[] matching = new int[m];
		Arrays.fill(matching, -1);;
		for (int i = 0; i < n; i++) {
			Arrays.fill(used, false);
			dfs(i, g, used, matching);
		}
		int res = ((int) Arrays.stream(matching).filter(v -> v != -1).count());
		System.out.println(res);
	}
}
