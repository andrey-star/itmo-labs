import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class E_Zhegalkin {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine().trim().split(" ")[0]);
		int arg = 1 << n;
		int[] f = new int[arg];
		for (int i = 0; i < arg; i++) {
			f[i] = Integer.parseInt(in.readLine().trim().split(" ")[1]);
		}
		int[][] triangle = new int[arg][arg];
		System.arraycopy(f, 0, triangle[0], 0, arg);
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
		for (int i = 0; i < arg; i++) {
			StringBuilder zeros = new StringBuilder();
			for (int j = 0; j < n; j++) {
				zeros.append("0");
			}
			String curBin = Integer.toBinaryString(i);
			curBin = zeros.append(curBin).substring(curBin.length());
			System.out.println(curBin + " " + result[i]);
		}
	}
	
}
