import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@SuppressWarnings("unchecked")
public class O_Chip {
	
	private static Pair[] indicesForWire;
	private static int[] wireAtIndex;
	
	private static void dfs1(List<Integer>[] g, int u, boolean[] used, List<Integer> topSort) {
		used[u] = true;
		for (int v : g[u]) {
			if (!used[v]) {
				dfs1(g, v, used, topSort);
			}
		}
		topSort.add(u);
	}
	
	private static void dfs2(List<Integer>[] g, int u, boolean[] used, List<Integer> comp) {
		used[u] = true;
		comp.add(u);
		for (int v : g[u]) {
			if (!used[v]) {
				dfs2(g, v, used, comp);
			}
		}
	}
	
	private static void findComps(List<Integer>[] g, List<Integer>[] r, int[] comp) {
		int n = g.length;
		boolean[] used = new boolean[n];
		List<Integer> topSort = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			if (!used[i]) {
				dfs1(g, i, used, topSort);
			}
		}
		Arrays.fill(used, false);
		int comps = 0;
		List<Integer> curComp = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			int u = topSort.get(n - 1 - i);
			if (!used[u]) {
				dfs2(r, u, used, curComp);
				for (int v : curComp) {
					comp[v] = comps;
				}
				comps++;
				curComp.clear();
			}
		}
	}
	
	private static int other(int index) {
		return indicesForWire[wireAtIndex[index]].a == index ?
				indicesForWire[wireAtIndex[index]].b :
				indicesForWire[wireAtIndex[index]].a;
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("chip.in"));
		int n = in.nextInt();
		int[] color = new int[n];
		indicesForWire = new Pair[n];
		wireAtIndex = new int[2 * n];
		for (int i = 0; i < n; i++) {
			color[i] = in.nextInt();
		}
		for (int i = 0; i < 2 * n; i++) {
			wireAtIndex[i] = in.nextInt() - 1;
			if (indicesForWire[wireAtIndex[i]] == null) {
				indicesForWire[wireAtIndex[i]] = new Pair();
				indicesForWire[wireAtIndex[i]].a = i;
			} else {
				indicesForWire[wireAtIndex[i]].b = i;
			}
		}
		List<Integer>[] g = new ArrayList[2 * n];
		List<Integer>[] r = new ArrayList[2 * n];
		for (int i = 0; i < 2 * n; i++) {
			g[i] = new ArrayList<>();
			r[i] = new ArrayList<>();
		}
		
		for (int curWireIndex = 0; curWireIndex < 2 * n; curWireIndex++) {
			int nextWireIndex = (curWireIndex + 1) % (2 * n);
			int wire = wireAtIndex[curWireIndex];
			int nextWire = wireAtIndex[nextWireIndex];
			if (color[wire] == color[nextWire]) {
				int otherCurWireIndex = other(curWireIndex);
				int otherNextWireIndex = other(nextWireIndex);
				g[otherCurWireIndex].add(nextWireIndex);
				g[otherNextWireIndex].add(curWireIndex);
				r[nextWireIndex].add(otherCurWireIndex);
				r[curWireIndex].add(otherNextWireIndex);
			}
		}
		
		boolean possible = true;
		int[] comp = new int[2 * n];
		findComps(g, r, comp);
		for (int i = 0; i < n; i++) {
			if (comp[indicesForWire[i].a] == comp[indicesForWire[i].b]) {
				possible = false;
				break;
			}
		}
		PrintWriter out = new PrintWriter(new File("chip.out"));
//		PrintWriter out = new PrintWriter(System.out);
		if (!possible) {
			out.println("NO");
		} else {
			out.println("YES");
			for (int i = 0; i < n; i++) {
				int res;
				if (comp[indicesForWire[i].a] < comp[indicesForWire[i].b]) {
					res = indicesForWire[i].a;
				} else {
					res = indicesForWire[i].b;
				}
				out.print(res + 1 + " ");
			}
		}
		out.close();
	}
	
	
	private static class Pair {
		int a;
		int b;
	}
}
