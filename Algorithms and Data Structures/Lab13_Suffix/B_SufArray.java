import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class B_SufArray {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s = in.readLine();
		in.close();
		int[] a = buildSufArray(s);
		PrintWriter out = new PrintWriter(System.out);
		for (int i = 1; i < a.length; i++) {
			out.print(a[i] + 1 + " ");
		}
		out.close();
	}
	
	private static int[] buildSufArray(String s) {
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
		
		return p;
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
	
	private static class Pair {
		char a;
		int b;
		
		public Pair(char a, int b) {
			this.a = a;
			this.b = b;
		}
		
	}
	
}
