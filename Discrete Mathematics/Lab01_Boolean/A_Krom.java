import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

public class A_Krom {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" ");
		int n = Integer.parseInt(line[0]);
		int[][] g = new int[2*n][2*n];
		for (int i = 0; i < g.length; i++) {
			Arrays.fill(g[i], Integer.MAX_VALUE);
			
		}
		int m = Integer.parseInt(line[1]);
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" ");
			int a = Integer.parseInt(line[0]);
			int b = Integer.parseInt(line[1]);
			if (a > 0 && b > 0) {
				g[n + a - 1][b - 1] = 1;
				g[n + b - 1][a - 1] = 1;
			} else if (a < 0 && b > 0) {
				g[-a - 1][b - 1] = 1;
				g[n + b - 1][n - a - 1] = 1;
			} else if (a < 0 && b < 0) {
				g[-a - 1][n - b - 1] = 1;
				g[-b - 1][n - a - 1] = 1;
			} else if (a > 0 && b < 0) {
				g[n + a - 1][n - b - 1] = 1;
				g[-b - 1][a - 1] = 1;
			}
		}
		
		for(int k = 0; k < 2*n; k++){
			for(int i = 0; i < 2*n; i++){
				for(int j = 0; j < 2*n; j++){
					if(g[i][k] != Integer.MAX_VALUE && g[k][j] != Integer.MAX_VALUE){
						if(g[i][j] > g[i][k] + g[k][j]){
							g[i][j] = g[i][k] + g[k][j];
						}
					}
				}
			}
		}
		
		boolean success = false;
		for (int i = 0; i < n; i++) {
			if (g[i][n + i] < Integer.MAX_VALUE && g[n + i][i] < Integer.MAX_VALUE) {
				success = true;
				break;
			}
		}
		if (success) {
			System.out.println("YES");
		} else {
			System.out.println("NO");
		}
	}
	
}
