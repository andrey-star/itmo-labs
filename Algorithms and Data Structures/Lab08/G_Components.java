import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class G_Components {
	
	private static void dfs(List<Integer>[] g, int u, boolean[] used, int curComp, int[] comp) {
		used[u] = true;
		comp[u] = curComp;
		for (int v : g[u]) {
			if (!used[v]) {
				dfs(g, v, used, curComp, comp);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("components.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
			g[b].add(a);
		}
		in.close();
		boolean[] used = new boolean[n];
		int[] comp = new int[n];
		int curComp = 0;
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs(g, i, used, ++curComp, comp);
			}
		}
		
		PrintWriter out = new PrintWriter(new File("components.out"));
		out.println(curComp);
		for (int i : comp) {
			out.print(i + " ");
		}
		out.close();
	}
}
