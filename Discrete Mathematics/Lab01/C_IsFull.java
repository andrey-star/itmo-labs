import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class C_IsFull {
	
	private static boolean isZeroPreserving(int[] f, int n) {
		return f[0] == 0;
	}
	
	private static boolean isOnePreserving(int[] f, int n) {
		return f[(1 << n) - 1] == 1;
	}
	
	private static boolean isSelfDual(int[] f, int n) {
		for (int i = 0; i < (1 << n); i++) {
			if (f[i ^ ((1 << n) - 1)] == f[i]) {
				return false;
			}
		}
		return true;
	}
	
	private static boolean isBigger(int a, int b, int n) {
		for (int i = 0; i < n; i++) {
			if (((a >> i) & 1) > ((b >> i) & 1)) {
				return true;
			}
		}
		return false;
	}
	
	private static boolean isMonotonous(int[] f, int n) {
		for (int i = 0; i < (1 << n); i++) {
			for (int j = i + 1; j < (1 << n); j++) {
				if (!isBigger(i, j, n)) {
					if (f[i] > f[j]) {
						return false;
					}
				}
			}
		}
		return true;
	}
	
	private static boolean isLinear(int[] table, int n) {
		int arg = 1 << n;
		int[][] triangle = new int[arg][arg];
		System.arraycopy(table, 0, triangle[0], 0, arg);
		for (int i = 1; i < arg; i++) {
			for (int j = 0; j < arg; j++) {
				if (i + j < arg) {
					triangle[i][j] = (triangle[i - 1][j] ^ triangle[i - 1][j + 1]);
				}
			}
		}
		int[] result = new int[arg];
		for (int i = 0; i < arg; i++) {
			result[i] = triangle[i][0];
			
		}
		for (int i = 0; i < (1 << n); i++) {
			if ((i & (i - 1)) != 0 && result[i] == 1) {
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int totalFunctions = Integer.parseInt(in.readLine().trim().split(" ")[0]);
		boolean hasNotZeroPr = false;
		boolean hasNotOnePr = false;
		boolean hasNotSelfDual = false;
		boolean hasNotMon = false;
		boolean hasNotLin = false;
		for (int q = 0; q < totalFunctions; q++) {
			String[] mas = in.readLine().trim().split(" ");
			int n = Integer.parseInt(mas[0]);
			String s = mas[1];
			int[] f = new int[1 << n];
			for (int i = 0; i < (1 << n); i++) {
				f[i] = s.charAt(i) - '0';
			}
			if (!isZeroPreserving(f, n)) {
				hasNotZeroPr = true;
			}
			if (!isOnePreserving(f, n)) {
				hasNotOnePr = true;
			}
			if (!isSelfDual(f, n)) {
				hasNotSelfDual = true;
			}
			if (!isMonotonous(f, n)) {
				hasNotMon = true;
			}
			if (!isLinear(f, n)) {
				hasNotLin = true;
			}
		}
		boolean isFull = hasNotZeroPr && hasNotOnePr && hasNotSelfDual && hasNotMon && hasNotLin;
		System.out.println(isFull ? "YES" : "NO");
	}
	
}
