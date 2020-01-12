import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public class C_Matching {
	
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
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("matching.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		
		Vertex[] vertices = new Vertex[n];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			int w = Integer.parseInt(line[i]);
			vertices[i] = new Vertex(i, w);
		}
		
		//noinspection unchecked
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int j = Integer.parseInt(line[0]);
			for (int k = 0; k < j; k++) {
				int b = Integer.parseInt(line[k + 1]) - 1;
				g[i].add(b);
			}
		}
		
		Arrays.sort(vertices, Comparator.comparing((Vertex v) -> v.w).reversed());
		boolean[] used = new boolean[n];
		int[] matching = new int[n];
		Arrays.fill(matching, -1);
		for (Vertex v : vertices) {
			Arrays.fill(used, false);
			dfs(v.index, g, used, matching);
		}
		
		int[] res = new int[n];
		Arrays.fill(res, -1);
		for(int i = 0; i < n; i++) {
			if (matching[i] != -1) {
				res[matching[i]] = i;
			}
		}
		PrintWriter out = new PrintWriter(new File("matching.out"));
		Arrays.stream(res).forEach(i -> out.print(i + 1 + " "));
		out.close();
	}
	
	private static class Vertex {
		int index;
		int w;
		
		Vertex(int index, int w) {
			this.index = index;
			this.w = w;
		}
		
		@Override
		public String toString() {
			return "Vertex{" +
					"index=" + index +
					", w=" + w +
					'}';
		}
	}
	
}
