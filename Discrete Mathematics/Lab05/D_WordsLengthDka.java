import java.io.*;
import java.util.Arrays;
import java.util.HashSet;

public class D_WordsLengthDka {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("problem4.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		int l = Integer.parseInt(line[3]);
		HashSet<Integer> term = new HashSet<>();
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			term.add(Integer.parseInt(line[i]) - 1);
		}
		int[][] g = new int[n]['z' - 'a' + 1];
		for (int[] ints : g) {
			Arrays.fill(ints, -1);
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			char c = line[2].charAt(0);
			g[a][c - 'a'] = b;
		}
		int[][] dp = new int[l + 1][n];
		dp[0][0] = 1;
		for (int i = 0; i < l; i++) {
			for (int j = 0; j < n; j++) {
				for (char c = 'a'; c <= 'z'; c++) {
					if (g[j][c - 'a'] != -1) {
						dp[i + 1][g[j][c - 'a']] += dp[i][j];
						dp[i + 1][g[j][c - 'a']] %= (int) 1e9 + 7;
					}
				}
			}
		}
		long sum = 0;
		for (int i = 0; i < n; i++) {
			if (term.contains(i)) {
				sum += dp[l][i];
			}
		}
		PrintWriter out = new PrintWriter(new File("problem4.out"));
		out.println(sum % ((int) 1e9 + 7));
		out.close();
	}
	
}