import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.*;

public class I_CodeLock {
	
	
	public static void main(String[] args) throws IOException {
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int d = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		StringBuilder res = new StringBuilder();
		if (k == 1) {
			for (int i = 0; i < d; i++) {
				res.append(i);
			}
		} else {
			List<Edge>[] brogn = buildBrogn(d, k - 1);
			res = new StringBuilder(getEuler(brogn, k));
		}
		System.out.println(res);
	}
	
	private static List<Edge>[] buildBrogn(int d, int l) {
		int n = 1;
		for (int i = 0; i < l; i++) {
			n *= d;
		}
		List<Edge>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < n; i++) {
			int removed = i % (n / d);
			for (int j = 0; j < d; j++) {
				int next = removed * d + j;
				g[i].add(new Edge(i, next, j));
			}
		}
		return g;
	}
	
	private static String getEuler(List<Edge>[] g, int k) {
		StringBuilder res = new StringBuilder();
		ArrayDeque<Integer> stack = new ArrayDeque<>();
		ArrayDeque<Integer> weight = new ArrayDeque<>();
		stack.push(0);
		weight.push(-1);
		mark: while (!stack.isEmpty()) {
			int u = stack.peek();
			List<Edge> edges = g[u];
			while (!edges.isEmpty()) {
				int last = edges.size() - 1;
				Edge e = edges.get(last);
				edges.remove(last);
				stack.push(e.v);
				weight.push(e.w);
				continue mark;
			}
			stack.pop();
			int cur = weight.pop();
			if (cur == -1) {
				for (int i = 0; i < k - 1; i++) {
					res.append(0);
				}
			} else {
				res.append(cur);
			}
		}
		res.reverse();
		return res.toString();
	}
	
	static class Edge {
		
		int u;
		int v;
		int w;
		
		Edge(int u, int v, int w) {
			this.u = u;
			this.v = v;
			this.w = w;
		}
	}
	
}