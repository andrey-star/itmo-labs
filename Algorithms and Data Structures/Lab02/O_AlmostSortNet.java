import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

public class O_AlmostSortNet {
	
	static class Pair {
		int a;
		int b;
		
		public Pair(int a, int b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public String toString() {
			return a + " " + b;
		}
	}
	
	private static ArrayList<Pair> buildSortNet(int[] a) {
		ArrayList<Pair> net = new ArrayList<>();
		int lastOne = 0;
		for (int i = 0; i < a.length - 1; i++) {
			if (a[i] == 1 && a[i + 1] == 0) {
				lastOne = i;
				break;
			}
		}
		// lastOne - index of comps - 2 element
		for (int i = lastOne - 1; i >= 0; i--) {
			// i - index of 0th el
			if (a[i] == 0) {
				for (int j = lastOne + 1; j >= i + 1; j--) {
					net.add(new Pair(i + 1, j + 1));
				}
			} else if (a[i] == 1) {
				for (int j = i; j <= lastOne; j++) {
					net.add(new Pair(j + 1, j + 2));
				}
			}
		}
		for (int i = lastOne + 2; i < a.length; i++) {
			// i - index of comps - 1 th el
			if (a[i] == 0) {
				for (int j = i - 1; j >= 0; j--) {
					net.add(new Pair(j + 1, j + 2));
				}
			} else if (a[i] == 1) {
				for (int j = 0; j <= i - 1; j++) {
					net.add(new Pair(j + 1, i + 1));
				}
			}
		}
		return net;
	}
	
	private static boolean isSorted(int[] a) {
		for (int i = 0; i < a.length - 1; i++) {
			if (a[i] == 1 && a[i + 1] == 0) {
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n;
		while ((n = Integer.parseInt(in.readLine())) != 0) {
			String[] line = in.readLine().trim().split(" +");
			int[] a = new int[n];
			for (int i = 0; i < n; i++) {
				a[i] = Integer.parseInt(line[i]);
			}
			if (!isSorted(a)) {
				ArrayList<Pair> net = buildSortNet(a);
				System.out.println(net.size());
				for (Pair p : net) {
					System.out.println(p.a + " " + p.b);
				}
			} else {
				System.out.println(-1);
			}
		}
	}
	
}
