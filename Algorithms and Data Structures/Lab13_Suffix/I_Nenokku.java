import java.io.*;
import java.util.*;

public class I_Nenokku {
	
	static int[] rmq;
	static int pw;
	private static String s;
	
	public static void main(String[] args) throws IOException {
		solve();
	}
	
	private static void solve() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String line;
		String[] split;
		List<Pair<String, Integer>> queries = new ArrayList<>();
		StringBuilder sb = new StringBuilder();
		while ((line = in.readLine()) != null) {
			line = line.toLowerCase();
			split = line.trim().split(" +");
			if (split[0].equals("?")) {
				queries.add(new Pair<>(split[1], sb.length()));
			} else {
				sb.append(split[1]);
			}
		}
		in.close();
		
		s = sb.toString();
		int[] suf = buildSufArray(s);
		buildRMQ(suf);
		PrintWriter out = new PrintWriter(System.out);
		for (Pair<String, Integer> query : queries) {
			if (suf.length == 0) {
				out.println("NO");
			} else {
				int left = binaryLeft(query.a, suf);
				int right = binaryRight(query.a, suf);
				if (left == -1 || right == -1) {
					out.println("NO");
				} else {
					int min = min(left, right);
					if (min + query.a.length() <= query.b) {
						out.println("YES");
					} else {
						out.println("NO");
					}
				}
			}
		}
		out.close();
	}
	
	private static int binaryLeft(String p, int[] suf) {
		int l = -1;
		int r = suf.length;
		while (l + 1 != r) {
			int mid = (l + r) / 2;
			if (compare(p, suf[mid]) > 0) {
				l = mid;
			} else {
				r = mid;
			}
		}
		if (r < suf.length && startsWith(suf[r], p)) {
			return r;
		}
		return -1;
	}
	
	private static int binaryRight(String p, int[] suf) {
		int l = -1;
		int r = suf.length;
		while (l + 1 != r) {
			int mid = (l + r) / 2;
			if (compare(p, suf[mid]) >= 0 || startsWith(suf[mid], p)) {
				l = mid;
			} else {
				r = mid;
			}
		}
		if (l >= 0 && startsWith(suf[l], p)) {
			return l;
		}
		return -1;
	}
	
	private static int compare(String p, int start) {
		int len1 = p.length();
		int len2 = s.length() - start;
		int lim = Math.min(len1, len2);
		
		int k = 0;
		while (k < lim) {
			char c1 = p.charAt(k);
			char c2 = s.charAt(start + k);
			if (c1 != c2) {
				return c1 - c2;
			}
			k++;
		}
		return len1 - len2;
	}
	
	private static boolean startsWith(int start, String p) {
		int to = start;
		int po = 0;
		int pc = p.length();
		if (start > s.length() - pc) {
			return false;
		}
		while (--pc >= 0) {
			if (s.charAt(to++) != p.charAt(po++)) {
				return false;
			}
		}
		return true;
	}
	
	private static int min(int l, int r) {
		int ans = Integer.MAX_VALUE;
		for (l += pw, r += pw + 1; l < r; l /= 2, r /= 2) {
			if (l % 2 == 1) {
				ans = Math.min(ans, rmq[l++]);
			}
			if (r % 2 == 1) {
				ans = Math.min(ans, rmq[--r]);
			}
		}
		return ans;
	}
	
	private static void buildRMQ(int[] a) {
		int n = a.length;
		pw = 1;
		while (pw < n) {
			pw *= 2;
		}
		rmq = new int[2 * pw];
		for (int i = 0; i < pw; i++) {
			rmq[pw + i] = i < n ? a[i] : Integer.MAX_VALUE;
		}
		for (int i = pw - 1; i > 0; i--) {
			rmq[i] = Math.min(rmq[2 * i], rmq[2 * i + 1]);
		}
	}
	
	private static int[] buildSufArray(String s) {
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
		cnt = new int[n];
		pp = new int[n];
		pos = new int[n];
		int k = 0;
		while ((1 << k) < n) {
			for (int i = 0; i < n; i++) {
				p[i] = (p[i] - (1 << k) + n) % n;
			}
			countSort(p, c);
			System.arraycopy(pp, 0, p, 0, n);
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
		int[] resP = new int[n - 1];
		System.arraycopy(p, 1, resP, 0, p.length - 1);
		return resP;
	}
	
	private static int[] cnt, pp, pos;
	
	private static int[] countSort(int[] p, int[] c) {
		int n = p.length;
		Arrays.fill(cnt, 0);
		for (int i : c) {
			cnt[i]++;
		}
		Arrays.fill(pp, 0);
		Arrays.fill(pos, 0);
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
	
}