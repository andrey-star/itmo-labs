import java.io.*;
import java.math.BigInteger;

public class Q_Tiles3 {
	
	private static int MOD;
	
	private static int solve(BigInteger n, int m) {
		int size = 1 << m;
		int[][] a = new int[size][size];
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				a[i][j] = 1;
				for (int k = 0; k < m - 1; k++) {
					int firstBitI = (i >> k) & 1;
					int secondBitI = (i >> (k + 1)) & 1;
					int firstBitJ = (j >> k) & 1;
					int secondBitJ = (j >> (k + 1)) & 1;
					if (firstBitI == secondBitI) {
						if (firstBitJ == secondBitJ && firstBitJ == firstBitI) {
							a[i][j] = 0;
							break;
						}
					}
				}
			}
		}
		
		int[][] res = pow(a, n.subtract(BigInteger.ONE));
		int sum = 0;
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				sum += res[i][j];
				if (sum >= MOD) {
					sum -= MOD;
				}
			}
		}
		return sum;
	}
	
	private static int[][] pow(int[][] a, BigInteger n) {
		int[][] res = new int[a.length][a.length];
		for (int i = 0; i < a.length; i++) {
			res[i][i] = 1;
		}
		while (n.compareTo(BigInteger.ZERO) > 0) {
			if (n.testBit(0)) {
				res = multiply(res, a);
			}
			n = n.shiftRight(1);
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
					if (res[i][j] >= MOD) {
						res[i][j] -= MOD;
					}
				}
			}
		}
		return res;
	}

	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("nice3.in")));
		PrintWriter out = new PrintWriter(new File("nice3.out"));
		String[] line = in.readLine().trim().split(" +");
		BigInteger n = new BigInteger(line[0]);
		int m = Integer.parseInt(line[1]);
		MOD = Integer.parseInt(line[2]);
		in.close();
		out.println(solve(n, m));
		out.close();
		
	}
	
}