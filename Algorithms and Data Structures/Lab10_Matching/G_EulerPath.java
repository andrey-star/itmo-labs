import java.io.*;
import java.util.*;

public class G_EulerPath {
	
	private static List<Integer> euler(List<Integer>[] g, List<Edge> edgess) {
		List<Integer> res = new ArrayList<>();
		ArrayDeque<Integer> stack = new ArrayDeque<>();
		stack.push(0);
		mark:
		while (!stack.isEmpty()) {
			int u = stack.peek();
			List<Integer> edges = g[u];
			while (!edges.isEmpty()) {
				int last = edges.size() - 1;
				int h = edges.get(last);
				Edge e = edgess.get(h);
				if (e.used) {
					edges.remove(last);
					continue;
				}
				int v = e.v;
				if (u == e.v) {
					v = e.u;
				}
				e.used = true;
				edges.remove(last);
				stack.push(v);
				continue mark;
			}
			int cur = stack.pop();
			res.add(cur);
		}
		return res;
	}
	
	public static void main(String[] args) throws IOException {
//      BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		//noinspection unchecked
		List<Integer>[] g = new ArrayList[n];
		List<Edge> edges = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int k = Integer.parseInt(line[0]);
			for (int j = 1; j <= k; j++) {
				int a = Integer.parseInt(line[j]) - 1;
				if (a < i) {
					continue;
				}
				Edge edge = new Edge(i, a, false);
				g[i].add(edges.size());
				g[a].add(edges.size());
				edges.add(edge);
			}
		}
		
		boolean[] used = new boolean[n];
		for (int i = 0; i < n; i++) {
			if (g[i].size() > 0) {
				dfs(i, g, edges, used);
				break;
			}
		}
		boolean hasPath = true;
		for (int i = 0; i < n; i++) {
			if (g[i].size() > 0 && !used[i]) {
				hasPath = false;
				break;
			}
		}
		
		PrintWriter out = new PrintWriter(System.out);
		if (!hasPath) {
			out.println(-1);
		} else {
			int oddOne = -1;
			int oddTwo = -1;
			for (int i = 0; i < n; i++) {
				if (g[i].size() % 2 == 1) {
					if (oddOne == -1) {
						oddOne = i;
					} else {
						oddTwo = i;
						break;
					}
				}
			}
			if (oddOne != -1) {
				Edge edge = new Edge(oddOne, oddTwo, false);
				g[oddOne].add(edges.size());
				g[oddTwo].add(edges.size());
				edges.add(edge);
			}
			
			List<Integer> euler = euler(g, edges);
			
			if (oddOne == -1) {
				out.println(euler.size() - 1);
				for (int i : euler) {
					out.print(i + 1 + " ");
				}
			} else {
				int start = -1;
				for (int i = 0; i < euler.size(); i++) {
					if ((euler.get(i) == oddOne && euler.get(i + 1) == oddTwo) ||
							(euler.get(i) == oddTwo && euler.get(i + 1) == oddOne)) {
						start = i + 1;
						break;
					}
				}
				out.println(euler.size() - 2);
				for (int i = start; i < euler.size(); i++) {
					out.print(euler.get(i) + 1 + " ");
				}
				for (int i = 1; i < start; i++) {
					out.print(euler.get(i) + 1 + " ");
				}
			}
			out.println();
		}
		out.close();
	}
	
	private static void dfs(int u, List<Integer>[] g, List<Edge> edges, boolean[] used) {
		used[u] = true;
		for (int h : g[u]) {
			Edge e = edges.get(h);
			int v = e.v;
			if (u == e.v) {
				v = e.u;
			}
			if (!used[v]) {
				dfs(v, g, edges, used);
			}
		}
	}
	
	static class Edge {
		
		int u;
		int v;
		boolean used;
		
		Edge(int u, int v, boolean used) {
			this.u = u;
			this.v = v;
			this.used = used;
		}
	}
}