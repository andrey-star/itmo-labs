import java.util.Arrays;

public class AllSortNet {
	
	static class Pair {
		int a;
		int b;
		
		Pair(int a, int b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public String toString() {
			return a + 1 + " " + (b + 1);
		}
	}
	
	private static int comps = 8;
	private static int rows = 5;
	private static Pair[] net = new Pair[comps];
	private static int amount = 0;
	
	private static void gen(int size) {
		if (size == comps) {
			amount++;
			if (isSortNet(net)) {
				System.out.println(Arrays.toString(net));
				System.exit(0);
			}
			return;
		}
		for (int i = 0; i < rows; i++) {
			for (int j = i + 1; j < rows; j++) {
				net[size] = new Pair(i, j);
				gen(size + 1);
			}
		}
	}
	
	private static boolean isSortNet(Pair[] net) {
		for (int i = 0; i < (1 << rows); i++) {
			int[] a = new int[rows];
			for (int j = 0; j < rows; j++) {
				a[j] = (i >> j) & 1;
			}
			sort(a, net);
			if (!isSorted(a)) {
				return false;
			}
		}
		return true;
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
		return true;
	}
	
	public static void main(String[] args) {
		long start = System.currentTimeMillis();
		gen(0);
		System.out.println(amount);
		System.out.println((System.currentTimeMillis() - start) / 1000.0);
	}
	
}
