import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class E_HasCycle {
	
	private static int start, end;
	
	private static boolean hasCycle(List<Integer>[] g, int u, int[] color, int[] p) {
		color[u] = 1;
		for (int v : g[u]) {
			if (color[v] == 0) {
				p[v] = u;
				if (hasCycle(g, v ,color, p)) {
					return true;
				}
			} else if (color[v] == 1) {
				start = v;
				end = u;
				return true;
			}
		}
		color[u] = 2;
		return false;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("cycle.in")));
		String[] line = in.readLine().trim().split(" +");
		int n  = Integer.parseInt(line[0]);
		int m  = Integer.parseInt(line[1]);
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a  = Integer.parseInt(line[0]) - 1;
			int b  = Integer.parseInt(line[1]) - 1;
			g[a].add(b);
		}
		in.close();
		
		int[] color = new int[n];
		int[] p = new int[n];
		Arrays.fill(p, -1);
		boolean hasCycle = false;
		for (int i = 0; i < n; i++) {
			if (color[i] == 0) {
				if (hasCycle(g, i, color, p)) {
					hasCycle = true;
					break;
				}
			}
		}
		
		PrintWriter out = new PrintWriter(new File("cycle.out"));
		if (!hasCycle) {
			out.println("NO");
		} else {
			out.println("YES");
			List<Integer> cycle = new ArrayList<>();
			int u = end;
			while (u != start) {
				cycle.add(u);
				u = p[u];
			}
			cycle.add(start);
			Collections.reverse(cycle);
			for (int v : cycle) {
				out.print(v + 1 + " ");
			}
		}
		out.close();
	}
}