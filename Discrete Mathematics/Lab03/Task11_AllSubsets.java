import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;
import java.util.TreeSet;

public class Task11_AllSubsets {
	
	static class Vector implements Comparable {
		int[] values;
		
		private Vector(int[] a) {
			values = Arrays.copyOf(a, a.length);
		}
		
		@Override
		public int compareTo(Object o) {
			Vector v = (Vector) o;
			for (int i = 0; i < values.length; i++) {
				if (values[i] != v.values[i]) {
					return values[i] - v.values[i];
				}
			}
			return 0;
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		Scanner in = new Scanner(new File("subsets.in"));
		int n = in.nextInt();
		in.close();
		PrintWriter out = new PrintWriter(new File("subsets.out"));
		TreeSet<Vector> set = new TreeSet<>();
		for (int i = 0; i < (1 << n); i++) {
			int[] a = new int[n];
			int cur = 0;
			for (int j = 0; j < n; j++) {
				if (((i >> (n - j - 1)) & 1) == 0) {
					a[cur++] = j + 1;
				}
			}
			set.add(new Vector(a));
		}
		for (Vector s : set) {
			Arrays.stream(s.values).forEach(i -> out.print(i != 0 ? i + " " : ""));
			out.println();
		}
		out.close();
	}
	
}