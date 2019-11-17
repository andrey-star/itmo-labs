import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class H_EdgePaths {
	
	private static List<Pair> euler(List<Integer>[] g, List<Edge> edgess) {
		List<Pair> res = new ArrayList<>();
		ArrayDeque<Pair> stack = new ArrayDeque<>();
		stack.push(new Pair(0, false));
		mark:
		while (!stack.isEmpty()) {
			int u = stack.peek().x;
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
				edges.remove(last);
				e.used = true;
				stack.push(new Pair(v, e.aux));
				continue mark;
			}
			res.add(stack.pop());
		}
		return res;
	}
	
	public static void main(String[] args) throws IOException {
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		List<Integer>[] g = new ArrayList[n];
		List<Edge> edges = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			Edge edge = new Edge(a, b, false, false);
			g[a].add(edges.size());
			g[b].add(edges.size());
			edges.add(edge);
		}
		
		
		int oddOne = -1;
		int oddTwo = -1;
		Map<Integer, Integer> odd = new HashMap<>();
		for (int i = 0; i < n; i++) {
			if (g[i].size() % 2 == 1) {
				if (oddOne == -1) {
					oddOne = i;
				} else {
					oddTwo = i;
					odd.put(oddOne, oddTwo);
					odd.put(oddTwo, oddOne);
					oddOne = -1;
					oddTwo = -1;
				}
			}
		}
		
		boolean[] used = new boolean[n];
		for (Map.Entry<Integer, Integer> uv : odd.entrySet()) {
			int u = uv.getKey();
			int v = uv.getValue();
			if (used[u] && used[v]) {
				continue;
			}
			used[u] = true;
			used[v] = true;
			Edge e = new Edge(u, v, false, true);
			g[u].add(edges.size());
			g[v].add(edges.size());
			edges.add(e);
		}
		
		
		List<Pair> euler = euler(g, edges);
		int start = -1;
		for (int i = 0; i < euler.size(); i++) {
			Pair cur = euler.get(i);
			if (cur.aux) {
				start = i + 1;
				break;
			}
		}
		
		PrintWriter out = new PrintWriter(System.out);
		if (start == -1) {
			if (euler.size() == 1) {
				out.println(0);
			} else {
				out.println(1);
				for (int i = 0; i < euler.size(); i++) {
					out.print(euler.get(i).x + 1 + " ");
				}
			}
		} else {
			List<List<Integer>> res = new ArrayList<>();
			int curPath = 0;
			res.add(new ArrayList<>());
			for (int i = start; i < euler.size(); i++) {
				Pair cur = euler.get(i);
				res.get(curPath).add(cur.x);
				if (cur.aux) {
					res.add(new ArrayList<>());
					curPath++;
				}
			}
			for (int i = 1; i < start; i++) {
				Pair cur = euler.get(i);
				res.get(curPath).add(cur.x);
			}
			
			out.println(res.size());
			for (List<Integer> re : res) {
				for (int i : re) {
					out.print(i + 1 + " ");
				}
				out.println();
			}
		}
		out.close();
	}
	
	static class Edge {
		
		int u;
		int v;
		boolean used;
		boolean aux;
		
		Edge(int u, int v, boolean used, boolean aux) {
			this.u = u;
			this.v = v;
			this.used = used;
			this.aux = aux;
		}
		
	}
	
	static class Pair {
		
		int x;
		boolean aux;
		
		Pair(int x, boolean aux) {
			this.x = x;
			this.aux = aux;
		}
	}
}
