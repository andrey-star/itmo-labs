import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Q_Truth {
	
	private static void dfs(List<Integer>[] g, int u, boolean[] used, int curComp, int[] comp) {
		used[u] = true;
		comp[u] = curComp;
		for (int v : g[u]) {
			if (!used[v]) {
				dfs(g, v, used, curComp, comp);
			}
		}
	}
	
	private static void findComps(List<Integer>[] g, boolean[] used, int[] comp) {
		int n = g.length;
		int curComp = 0;
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs(g, i, used, ++curComp, comp);
			}
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("truth.in"));
		int n = in.nextInt();
		int m = in.nextInt();
		Statement[] st = new Statement[m];
		for (int i = 0; i < m; i++) {
			int a = in.nextInt() - 1;
			int b = in.nextInt() - 1;
			boolean c = in.next().equals("T");
			st[i] = new Statement(a, b, c);
		}
		
		List<Integer>[] g = new ArrayList[2 * n];
		boolean[] used = new boolean[2 * n];
		int[] comp = new int[2 * n];
		for (int i = 0; i < 2 * n; i++) {
			g[i] = new ArrayList<>();
		}
		int left = 0;
		int right = m + 1;
		while (left != right - 1) {
			int mid = (left + right) / 2;
			for (int i = 0; i < mid; i++) {
				Statement s = st[i];
				if (s.c) {
					g[s.a].add(s.b);
					g[s.b].add(s.a);
					g[n + s.a].add(n + s.b);
					g[n + s.b].add(n + s.a);
				} else {
					g[s.a].add(n + s.b);
					g[s.b].add(n + s.a);
					g[n + s.a].add(s.b);
					g[n + s.b].add(s.a);
				}
				
			}
			boolean possible = true;
			findComps(g, used, comp);
			for (int i = 0; i < n; i++) {
				if (comp[i] == comp[n + i]) {
					possible = false;
					break;
				}
			}
			if (possible) {
				left = mid;
			} else {
				right = mid;
			}
			for (int i = 0; i < 2 * n; i++) {
				g[i].clear();
				used[i] = false;
				comp[i] = 0;
			}
		}
		PrintWriter out = new PrintWriter(new File("truth.out"));
//		PrintWriter out = new PrintWriter(System.out);
		out.println(left);
		out.close();
	}
	
	static class Statement {
		int a;
		int b;
		boolean c;
		
		Statement(int a, int b, boolean c) {
			this.a = a;
			this.b = b;
			this.c = c;
		}
	}
}
