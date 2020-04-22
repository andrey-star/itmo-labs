import java.io.*;
import java.util.Random;

public class P_ReversePrefixFunction {
	
	private static final int MOD = (int) (1e9 + 7);
	
	private static void test() throws FileNotFoundException {
		Random random = new Random();
		PrintWriter out = new PrintWriter(new File("test.in"));
		int n = 10;
//		int c = random.nextInt((int) 1e9);
		int c = 3;
		out.println(n + " " + c);
		for (int i = 1; i < n; i++) {
			out.print(random.nextInt(i + 1) + " ");
		}
		out.close();
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int c = Integer.parseInt(line[1]);
		line = in.readLine().trim().split(" +");
		int[] a = new int[n + 1];
		a[0] = -1;
		for (int i = 2; i < n + 1; i++) {
			a[i] = Integer.parseInt(line[i - 2]);
		}
		long res = c;
		for (int i = 2; i < n + 1; i++) {
			if (a[i] == 0) {
				int k = i - 1;
				int cur = c;
				while (k > 0) {
					k = a[k];
					if (a[k + 1] == 0) {
						cur--;
					}
				}
				res *= cur;
				res %= MOD;
			}
		}
		System.out.println(res);
//		}
	}
	
}
