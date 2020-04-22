import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class M_CheckSortNet {
	
	static class Pair {
		int a;
		int b;
		
		Pair(int a, int b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public String toString() {
			return "Pair{" +
					"a=" + a +
					", b=" + b +
					'}';
		}
	}
	
	private static void sort(int[] a, Pair[] net) {
		for (Pair comp : net) {
			int min = Math.min(a[comp.a], a[comp.b]);
			int max = Math.max(a[comp.a], a[comp.b]);
			a[Math.min(comp.a, comp.b)] = min;
			a[Math.max(comp.a, comp.b)] = max;
		}
	}
	
	private static boolean isSorted(int[] a) {
		for (int i = 0; i < a.length - 1; i++) {
			if (a[i + 1] < a[i]) {
				return false;
			}
		}
		return  true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" ");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		int k = Integer.parseInt(line[2]);
		int curPos = 0;
		Pair[] net = new Pair[m];
		for (int i = 0; i < k; i++) {
			line = in.readLine().trim().split(" ");
			int r = Integer.parseInt(line[0]);
			for (int j = 0; j < r; j++) {
				int a = Integer.parseInt(line[2*j + 1]) - 1;
				int b = Integer.parseInt(line[2*j + 2]) - 1;
				net[curPos++] = new Pair(a, b);
			}
		}
		boolean isSortNet = true;
		for (int i = 0; i < (1 << n); i++) {
			int[] a = new int[n];
			for (int j = 0; j < n; j++) {
				a[j] = (i >> j) & 1;
			}
			sort(a, net);
			if (!isSorted(a)) {
				isSortNet = false;
				break;
			}
		}
		System.out.println(isSortNet ? "Yes" : "No");
	}
	
}
