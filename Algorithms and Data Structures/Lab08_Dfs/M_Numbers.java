import java.io.*;
import java.util.*;

public class M_Numbers {
	
	private static boolean topSortHasCycle(List<Integer>[] g, int u, int[] color, List<Integer> topSort) {
		color[u] = 1;
		for (int v : g[u]) {
			if (color[v] == 0) {
				topSortHasCycle(g, v ,color, topSort);
			}
			if (color[v] == 1) {
				return true;
			}
		}
		color[u] = 2;
		topSort.add(u);
		return false;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("tiv.in")));
		int n = 26;
		int m = Integer.parseInt(in.readLine());
		String[] numbers = new String[m];
		for (int i = 0; i < m; i++) {
			numbers[i] = in.readLine();
		}
		in.close();
		
		List<Integer>[] g = new ArrayList[n];
		for (int i = 0; i < n; i++) {
			g[i] = new ArrayList<>();
		}
		Set<Integer> notZero = new HashSet<>();
		Set<Integer> onlyOut = new HashSet<>();
		for (int i = 0; i < n; i++) {
			onlyOut.add(i);
		}
		try (PrintWriter out = new PrintWriter(new File("tiv.out"))) {
//		try (PrintWriter out = new PrintWriter(System.out)) {
			for (int i = 0; i < m - 1; i++) {
				String fir = numbers[i];
				String sec = numbers[i + 1];
				if (fir.length() > sec.length()) {
					out.println("No");
					return;
				}
				
				if (fir.length() > 1) {
					notZero.add(fir.charAt(0) - 'a');
				}
				if (sec.length() > 1) {
					notZero.add(sec.charAt(0) - 'a');
				}
				if (fir.length() == sec.length()) {
					int pos = 0;
					while (fir.charAt(pos) == sec.charAt(pos)) {
						pos++;
						if (pos == fir.length()) {
							out.println("No");
							return;
						}
					}
					g[fir.charAt(pos) - 'a'].add(sec.charAt(pos) - 'a');
					onlyOut.remove(sec.charAt(pos) - 'a');
				}
			}
			
			int[] color = new int[n];
			List<Integer> topSort = new ArrayList<>();
			for (int i = 0; i < n; i++) {
				if (color[i] == 0) {
					if (topSortHasCycle(g, i, color, topSort)) {
						out.println("No");
						return;
					}
				}
			}
			int zero = -1;
			for (int i : onlyOut) {
				if (!notZero.contains(i)) {
					zero = i;
					break;
				}
			}
			if (zero == -1) {
				out.println("No");
				return;
			}
			int[] res = new int[n];
			Collections.reverse(topSort);
			for (int i = 0; i < n; i++) {
				res[topSort.get(i)] = i;
			}
			for (int i = 0; i < n; i++) {
				if (res[i] < res[zero]) {
					res[i]++;
				}
			}
			res[zero] = 0;
			out.println("Yes");
			for (int re : res) {
				out.print(re + " ");
			}
		}
	}
	
}