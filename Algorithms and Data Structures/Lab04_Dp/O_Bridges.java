import java.io.*;

public class O_Bridges {
	
	private static int dp(int x, int a, int y, int b, int l) {
		int[][] dp = new int[x + 1][y + 1];
		for (int i = 0; i < x + 1; i++) {
			for (int j = 0; j < y + 1; j++) {
				for (int k = 0; k <= i; k++) {
					int minLogs = (int) Math.ceil(1.0 * (l - k * a) / b);
					minLogs = Math.max(minLogs, 0);
					if (minLogs <= j) {
						dp[i][j] = Math.max(dp[i][j], dp[i - k][j - minLogs] + 1);
					}
				}
			}
		}
		return dp[x][y];
	}
	
	private static long binSearch(int x, int a, int y, int b, int l) {
		int left = 0;
		int right = Integer.MAX_VALUE;
		int rowWidth;
		while (left != right - 1) {
			rowWidth = left + (right - left) / 2;
			int res = dp(x, a, y, b, rowWidth); // max length of bridge with given row width
			if (res < l) { // bridge too short => row too wide
				right = rowWidth;
			} else {
				left = rowWidth;
			}
		}
		return left;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("bridge.in")));
		String[] line = in.readLine().trim().split(" +");
		int x = Integer.parseInt(line[0]);
		int a = Integer.parseInt(line[1]); // digitsSum logs len a
		int y = Integer.parseInt(line[2]);
		int b = Integer.parseInt(line[3]); // modN logs len b
		int l = Integer.parseInt(line[4]);
		in.close();
		PrintWriter out = new PrintWriter(new File("bridge.out"));
		out.println(binSearch(x, a, y, b, l));
		out.close();
	}
}