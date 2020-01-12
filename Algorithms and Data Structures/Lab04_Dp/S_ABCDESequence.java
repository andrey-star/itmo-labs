import java.io.*;

public class S_ABCDESequence {
	
	private static final int MOD = 999999937;
	
	private static int solve(long n) {
		int[][] a = new int[][]{{1, 1, 1, 1, 1}, {1, 1, 1, 1, 1}, {1, 1, 1, 1, 1}, {1, 1, 0, 1, 0}, {1, 1, 0, 1, 0}};
		int[][] res = pow(a, n - 1);
		int sum = 0;
		for (int i = 0; i < 5; i++) {
			for (int j = 0; j < 5; j++) {
				sum = (sum + res[i][j]) % MOD;
			}
		}
		return sum;
	}
	
	private static int[][] pow(int[][] a, long n) {
		int[][] res = new int[a.length][a.length];
		for (int i = 0; i < a.length; i++) {
			res[i][i] = 1;
		}
		while (n > 0) {
			if (n % 2 == 1) {
				res = multiply(res, a);
			}
			n /= 2;
			a = multiply(a, a);
		}
		return res;
	}
	
	private static int[][] multiply(int[][] a, int[][] b) {
		int[][] res = new int[a.length][a.length];
		for (int i = 0; i < a.length; i++) {
			for (int j = 0; j < a.length; j++) {
				for (int k = 0; k < a.length; k++) {
					res[i][j] += ((long) a[i][k] * b[k][j]) % MOD;
					res[i][j] %= MOD;
				}
			}
		}
		return res;
	}

	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("sequences.in")));
		PrintWriter out = new PrintWriter(new File("sequences.out"));
		String line;
		while (!(line = in.readLine()).equals("0")) {
			long n = Long.parseLong(line);
			out.println(solve(n));
		}
		in.close();
		out.close();
		
	}
	
}