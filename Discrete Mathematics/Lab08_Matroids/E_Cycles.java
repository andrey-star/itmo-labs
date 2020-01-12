import java.io.*;
import java.util.*;

public class E_Cycles {
	
	private static void gen(Set<Long> dependent, long set, int cur, int n) {
		if (cur == n) {
			dependent.add(set);
			return;
		}
		if (((set >> cur) & 1) == 0) {
			gen(dependent, set + (1 << cur), cur + 1, n);
		}
		gen(dependent, set, cur + 1, n);
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("cycles.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		Element[] elements = new Element[n];
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < n; i++) {
			elements[i] = new Element(i, Integer.parseInt(line[i]));
		}
		Set<Long> cycles = new HashSet<>();
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int k = Integer.parseInt(line[0]);
			long cycle = 0;
			for (int j = 1; j <= k; j++) {
				int a = Integer.parseInt(line[j]) - 1;
				cycle += (1 << a);
			}
			cycles.add(cycle);
			
		}
		Set<Long> dependent = new HashSet<>();
		for (long cycle : cycles) {
			gen(dependent, cycle, 0, n);
		}
		
		Arrays.sort(elements, Comparator.comparingInt((Element e) -> e.w).reversed());
		
		long res = 0;
		long base = 0;
		for (Element element : elements) {
			base += (1 << element.index);
			if (!dependent.contains(base)) {
				res += element.w;
			} else {
				base -= (1 << element.index);
			}
		}
		PrintWriter out = new PrintWriter(new File("cycles.out"));
		out.println(res);
		out.close();
	}
	
	private static class Element {
		
		int index;
		int w;
		
		Element(int index, int w) {
			this.index = index;
			this.w = w;
		}
	}
	
}
