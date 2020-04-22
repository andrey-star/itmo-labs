import java.io.*;
import java.util.*;

public class D_Check {
	
	private static boolean first(Set<BitSet> sets, int n) {
		return sets.contains(new BitSet(n));
	}
	
	private static boolean second(Set<BitSet> sets, BitSet s, Map<BitSet, Boolean> mem) {
		if (mem.containsKey(s)) {
			return mem.get(s);
		}
		if (!sets.contains(s)) {
			mem.put(s, false);
			return false;
		}
		boolean res = true;
		for (int i = 0; i < s.size(); i++) {
			if (s.get(i)) {
				BitSet next = bitSet(s);
				next.set(i, false);
				res &= second(sets, next, mem);
			}
		}
		mem.put(s, res);
		return res;
	}
	
	private static boolean second(Set<BitSet> sets) {
		Map<BitSet, Boolean> mem = new HashMap<>();
		for (BitSet set : sets) {
			if (!second(sets, set, mem)) {
				return false;
			}
		}
		return true;
	}
	
	private static boolean third(Set<BitSet> sets, BitSet a, BitSet b) { // b > a
		BitSet diff = bitSet(b);
		diff.andNot(a);
		for (int i = 0; i < diff.size(); i++) {
			if (diff.get(i)) {
				a.set(i);
				if (sets.contains(a)) {
					a.set(i, false);
					return true;
				}
				a.set(i, false);
			}
		}
		return false;
	}
	
	private static boolean third(Set<BitSet> sets) {
		for (BitSet set1 : sets) {
			for (BitSet set2 : sets) {
				int set1size = set1.cardinality();
				int set2size = set2.cardinality();
				if (set1size < set2size) {
					if (!third(sets, set1, set2)) {
						return false;
					}
				}
			}
		}
		return true;
	}
	
	private static boolean check(Set<BitSet> sets, int n) {
		boolean res = first(sets, n);
		if (res) {
			res = second(sets);
		}
		if (res) {
			res = third(sets);
		}
		return res;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("check.in")));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		Set<BitSet> sets = new HashSet<>();
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int k = Integer.parseInt(line[0]);
			BitSet set = new BitSet(n);
			for (int j = 1; j <= k; j++) {
				int a = Integer.parseInt(line[j]) - 1;
				set.set(a);
			}
			sets.add(set);
		}
		PrintWriter out = new PrintWriter(new File("check.out"));
		out.println(check(sets, n) ? "YES" : "NO");
		out.close();
	}
	
	private static BitSet bitSet(BitSet s) {
		BitSet res = new BitSet(s.size());
		for (int i = 0; i < s.size(); i++) {
			if (s.get(i)) {
				res.set(i);
			}
		}
		return res;
	}
}