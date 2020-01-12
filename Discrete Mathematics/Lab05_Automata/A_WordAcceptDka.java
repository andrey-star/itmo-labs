import java.io.*;
import java.util.Arrays;

public class A_WordAcceptDka {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("problem1.in")));
		String s = in.readLine();
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		int[][] g = new int[n]['z' - 'a' + 1];
		for (int[] ints : g) {
			Arrays.fill(ints, -1);
		}
		int[] term = new int[k];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			term[i] = Integer.parseInt(line[i]) - 1;
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			char c = line[2].charAt(0);
			g[a][c - 'a'] = b;
		}
		int cur = 0;
		boolean ok = true;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (g[cur][c - 'a'] == -1) {
				ok = false;
				break;
			}
			cur = g[cur][c - 'a'];
		}
		PrintWriter out = new PrintWriter(new File("problem1.out"));
		if (!ok) {
			out.println("Rejects");
		} else {
			ok = false;
			for (int i = 0; i < k; i++) {
				if (term[i] == cur) {
					ok = true;
					break;
				}
			}
			if (ok) {
				out.println("Accepts");
			} else {
				out.println("Rejects");
			}
		}
		out.close();
	}
	
}
