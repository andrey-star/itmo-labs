import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.*;

public class Ba9d extends AbstractTask {
	
	public Ba9d() {
		super("ba9d");
	}
	
	public static void main(String[] args) throws IOException {
		new Ba9d().test();
	}
	
	public void test(Path test) throws FileNotFoundException {
		System.out.println(test.toFile());
		Scanner in = new Scanner(new InputStreamReader(new FileInputStream(test.toFile())));
		String s = in.next();
		System.out.println(solve(s));
	}
	
	private String solve(String s) {
		Pair2 sufLcp = buildSufArray(s);
		int[] suf = sufLcp.p;
		int[] lcp = sufLcp.lcp;
		int maxIndex = 0;
		for (int i = 0; i < lcp.length; i++) {
			if (lcp[i] > lcp[maxIndex]) {
				maxIndex = i;
			}
		}
		return s.substring(suf[maxIndex], suf[maxIndex] + lcp[maxIndex]);
	}
	
	private Pair2 buildSufArray(String s) {
		s += "#";
		int n = s.length();
		int[] p = new int[n];
		int[] c = new int[n];
		Pair[] a = new Pair[n];
		for (int i = 0; i < n; i++) {
			a[i] = new Pair(s.charAt(i), i);
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
		return new Pair2(p, lcp);
	}
	
	private int[] countSort(int[] p, int[] c) {
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
	
	private static class Pair {
		char a;
		int b;
		
		public Pair(char a, int b) {
			this.a = a;
			this.b = b;
		}
	}
	
	private static class Pair2 {
		int[] p;
		int[] lcp;
		
		public Pair2(int[] p, int[] lcp) {
			this.p = p;
			this.lcp = lcp;
		}
	}
	
}
