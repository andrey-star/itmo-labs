import java.io.*;
import java.util.Arrays;
import java.util.Comparator;

public class E_Refrain {
	
	public static void main(String[] args) throws IOException {
		solve();
	}
	
	private static void solve() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		line = in.readLine().trim().split(" +");
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < n; i++) {
			sb.append(Integer.parseInt(line[i]) - 1);
		}
		String s = sb.toString();
		Pair<int[], int[]> pair = buildSufArray(s);
		int[] suf = pair.a;
		int[] lcp = pair.b;
		//noinspection unchecked
		Pair<Integer, Integer>[] indexedLcp = new Pair[lcp.length - 1];
		for (int i = 1; i < lcp.length; i++) {
			indexedLcp[i - 1] = new Pair<>(i - 1, lcp[i]);
		}
		Arrays.sort(indexedLcp, Comparator.comparing((Pair<Integer, Integer> p) -> p.b).reversed());
		
		long max = n;
		int len = n;
		int start = 0;
		DSU dsu = new DSU(lcp.length);
		for (Pair<Integer, Integer> lcpp : indexedLcp) {
			int place = lcpp.a;
			dsu.union(place, place + 1);
			int size = dsu.size(place);
			long newVal = (long) lcpp.b * size;
			if (max < newVal) {
				max = newVal;
				len = lcpp.b;
				start = suf[lcpp.a];
			}
		}
		PrintWriter out = new PrintWriter(System.out);
		out.println(max);
		out.println(len);
		for (int i = start; i < start + len; i++) {
			out.print(Integer.parseInt("" + s.charAt(i)) + 1 + " ");
		}
		out.close();
	}
	
	private static Pair<int[], int[]> buildSufArray(String s) {
		s += "#";
		int n = s.length();
		int[] p = new int[n];
		int[] c = new int[n];
		//noinspection unchecked
		Pair<Character, Integer>[] a = new Pair[n];
		for (int i = 0; i < n; i++) {
			a[i] = new Pair<>(s.charAt(i), i);
		}
		Arrays.sort(a, Comparator.comparing(pp -> pp.a));
		for (int i = 0; i < n; i++) {
			p[i] = a[i].b;
		}
		for (int i = 1; i < n; i++) {
			c[p[i]] = c[p[i - 1]];
			if (a[i].a != a[i - 1].a) {
				c[p[i]]++;
			}
		}
		int[] cc = new int[n];
		int k = 0;
		while ((1 << k) < n) {
			for (int i = 0; i < n; i++) {
				p[i] = (p[i] - (1 << k) + n) % n;
			}
			p = countSort(p, c);
			for (int i = 0; i < n; i++) {
				cc[i] = 0;
			}
			for (int i = 1; i < n; i++) {
				cc[p[i]] = cc[p[i - 1]];
				if (c[p[i - 1]] != c[p[i]] || c[(p[i - 1] + (1 << k)) % n] != c[(p[i] + (1 << k)) % n]) {
					cc[p[i]]++;
				}
			}
			System.arraycopy(cc, 0, c, 0, n);
			k++;
		}
		
		int[] lcp = new int[n];
		k = 0;
		for (int i = 0; i < n - 1; i++) {
			int pIndex = c[i];
			int j = p[pIndex - 1];
			while (s.charAt(i + k) == s.charAt(j + k)) {
				k++;
			}
			lcp[pIndex] = k;
			k = Math.max(k - 1, 0);
		}
		int[] resLcp = new int[n - 1];
		System.arraycopy(lcp, 1, resLcp, 0, lcp.length - 1);
		int[] resP = new int[n - 1];
		System.arraycopy(p, 1, resP, 0, p.length - 1);
		return new Pair<>(resP, resLcp);
	}
	
	private static int[] countSort(int[] p, int[] c) {
		int n = p.length;
		int[] cnt = new int[n];
		for (int i : c) {
			cnt[i]++;
		}
		int[] pp = new int[n];
		int[] pos = new int[n];
		pos[0] = 0;
		for (int i = 1; i < n; i++) {
			pos[i] = pos[i - 1] + cnt[i - 1];
			
		}
		for (int i : p) {
			int k = c[i];
			pp[pos[k]] = i;
			pos[k]++;
		}
		return pp;
	}
	
	private static class Pair<A, B> {
		A a;
		B b;
		
		public Pair(A a, B b) {
			this.a = a;
			this.b = b;
		}
	}
	
	private static class DSU {
		
		private int[] parent;
		private int[] size;
		private int[] start;
		
		private DSU(int n) {
			parent = new int[n];
			size = new int[n];
			start = new int[n];
			for (int i = 0; i < n; i++) {
				parent[i] = i;
				size[i] = 1;
				start[i] = i;
			}
		}
		
		private void union(int x, int y) {
			x = find(x);
			y = find(y);
			if (x != y) {
				size[y] += size[x];
				start[y] = Math.min(start[x], start[y]);
				parent[x] = y;
			}
		}
		
		private int find(int index) {
			if (parent[index] != index) {
				parent[index] = find(parent[index]);
			}
			return parent[index];
		}
		
		@Override
		public String toString() {
			StringBuilder s = new StringBuilder();
			for (int i = 0; i < parent.length; i++) {
				s.append(find(i)).append(" ");
			}
			return s.toString();
		}
		
		public int size(int place) {
			return size[find(place)];
		}
		
		public int size() {
			return parent.length;
		}
		
		public int start(int x) {
			return start[find(x)];
		}
	}
	
	
}
