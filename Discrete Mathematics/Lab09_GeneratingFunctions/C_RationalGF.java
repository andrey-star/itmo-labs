import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class C_RationalGF {
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int k = Integer.parseInt(line[0]);
		long[] a = new long[k];
		int[] c = new int[k];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			a[i] = Integer.parseInt(line[i]);
		}
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			c[i] = Integer.parseInt(line[i]);
		}
		in.close();
		
		long[] q = new long[k + 1];
		q[0] = 1;
		for (int i = 1; i < q.length; i++) {
			q[i] = -c[i - 1];
		}
		print(p(a, c));
		print(q);
	}
	
	private static long[] p(long[] a, int[] c) {
		int n = a.length;
		long[] p = new long[n];
		for (int k = 0; k < n; k++) {
			long actual = 0;
			for (int i = 1; i <= n; i++) {
				actual += c[i - 1] * get(k - i, a);
			}
			p[k] = a[k] - actual;
		}
		return p;
	}
	
	private static long get(int i, long[] a) {
		return 0 <= i && i < a.length ? a[i] : 0;
	}
	
	private static void print(long[] a) {
		int power = 0;
		for (int i = a.length - 1; i >= 0; i--) {
			if (a[i] != 0) {
				power = i;
				break;
			}
		}
		System.out.println(power);
		print(a, power + 1);
	}
	
	private static void print(long[] a, int amount) {
		for (int i = 0; i < amount; i++) {
			System.out.print(get(i, a) + " ");
		}
		System.out.println();
	}
}
