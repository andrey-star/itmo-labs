import java.io.*;
import java.util.*;

public class H_SlowMinimization {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("minimization.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]) + 1;
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		HashSet<Integer> term = new HashSet<>();
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			term.add(Integer.parseInt(line[i]));
		}
		int[][] g = new int[n]['z' - 'a' + 1];
		//noinspection unchecked
		List<Integer>[][] gRev = new ArrayList[n][26];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < 26; j++) {
				gRev[i][j] = new ArrayList<>();
			}
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			char c = line[2].charAt(0);
			g[a][c - 'a'] = b;
		}
		for (int i = 0; i < n; i++) {
			for (int c = 0; c < 26; c++) {
				gRev[g[i][c]][c].add(i);
			}
		}
//      for (int i = 0; i < n; i++) {
//          for (int j = 0; j < 26; j++) {
//              System.out.println((i + 1) + " " + (char) ('a' + j) + " " + gRev[i][j].stream().map(v -> v + 1).collect(Collectors.toList()));
//          }
//      }
		boolean[] reachableFromStart = new boolean[n];
		markReachable(1, g, reachableFromStart);
		Pair2 p2 = minimize(n, term, gRev, reachableFromStart);
		int[] component = p2.x;
		int count = p2.y;
		int[][] gNew = new int[count][26];
		for (int i = 0; i < count; i++) {
			for (int c = 0; c < 26; c++) {
				gNew[i][c] = -1;
			}
		}
		Set<Integer> terminalSet = new HashSet<>();
		for (int i = 0; i < n; i++) {
			if (term.contains(i) && component[i] != -1) {
				terminalSet.add(component[i]);
			}
			for (int c = 0; c < 26; c++) {
				int edge = g[i][c];
				if (component[i] != -1 && component[edge] != 0) {
					gNew[component[i]][c] = component[edge];
				}
			}
		}
		StringBuilder terminals = new StringBuilder();
		for (Integer i : terminalSet) {
			terminals.append(i).append(" ");
		}
		int edges = 0;
		StringBuilder res = new StringBuilder();
		for (int i = 0; i < gNew.length; i++) {
			for (int c = 0; c < gNew[i].length; c++) {
				if (gNew[i][c] != -1) {
					edges++;
					res.append(i).append(" ").append(gNew[i][c]).append(" ").append((char) ('a' + c)).append("\n");
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("minimization.out"));
		out.println(gNew.length - 1 + " " + edges + " " + terminalSet.size() + "\n" + terminals + "\n" + res);
		out.close();
	}
	
	private static void markReachable(int u, int[][] g, boolean[] mark) {
		mark[u] = true;
		for (int i = 0; i < 'z' - 'a' + 1; i++) {
			int v = g[u][i];
			if (v != -1 && !mark[v]) {
				markReachable(v, g, mark);
			}
		}
	}
	
	private static Pair2 minimize(int n, HashSet<Integer> term, List<Integer>[][] g, boolean[] reachableFromStart) {
		List<Pair> q = new LinkedList<>();
		boolean[][] equivMat = new boolean[n][n];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (!equivMat[i][j] && (term.contains(i) != term.contains(j))) {
					equivMat[i][j] = true;
					equivMat[j][i] = true;
					q.add(new Pair(i, j));
				}
			}
		}
		while (!q.isEmpty()) {
			Pair uv = q.remove(0);
			int u = uv.x;
			int v = uv.y;
			for (int c = 0; c < 'z' - 'a' + 1; c++) {
				for (int i = 0; i < g[u][c].size(); i++) {
					int u1 = g[u][c].get(i);
					for (int j = 0; j < g[v][c].size(); j++) {
						int v1 = g[v][c].get(j);
						if (!equivMat[u1][v1]) {
							equivMat[u1][v1] = true;
							equivMat[v1][u1] = true;
							q.add(new Pair(u1, v1));
						}
					}
				}
			}
		}
		int[] comp = new int[n];
		Arrays.fill(comp, -1);
		for (int i = 0; i < n; i++) {
			if (!equivMat[0][i]) {
				comp[i] = 0;
			}
		}
		int comps = 0;
		for (int i = 1; i < n; i++) {
			if (!reachableFromStart[i]) {
				continue;
			}
			if (comp[i] == -1) {
				comps++;
				comp[i] = comps;
				for (int j = i + 1; j < n; j++) {
					if (!equivMat[i][j]) {
						comp[j] = comps;
					}
				}
			}
		}
		return new Pair2(comp, comps + 1);
	}
	
	static class Pair {
		int x;
		int y;
		
		Pair(int x, int y) {
			this.x = x;
			this.y = y;
		}
		
		@Override
		public String toString() {
			return "x=" + x +
					", y=" + y;
		}
	}
	
	static class Pair2 {
		int[] x;
		int y;
		
		Pair2(int[] x, int y) {
			this.x = x;
			this.y = y;
		}
		
	}
}