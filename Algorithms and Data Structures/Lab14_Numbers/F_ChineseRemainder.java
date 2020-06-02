import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class F_ChineseRemainder {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		Quad[] a = new Quad[n];
		String[] line;
		for (int i = 0; i < n; i++) {
			line = in.readLine().trim().split(" +");
			int ai = Integer.parseInt(line[0]);
			int bi = Integer.parseInt(line[1]);
			int ni = Integer.parseInt(line[2]);
			int mi = Integer.parseInt(line[3]);
			if (mi != 1) {
				a[i] = new Quad(ai, bi, ni, mi);
			} else {
				a[i] = new Quad(bi, ai, mi, ni);
			}
			
		}
		in.close();
		solve(a);
	}
	
	private static void solve(Quad[] arr) {
		PrintWriter out = new PrintWriter(System.out);
		for (Quad q : arr) {
			long k = (q.b - q.a) * rev(q.n, q.m);
			k = mod(k, q.m);
			out.println(q.a + q.n * k);
		}
		out.close();
	}
	
	private static long rev(long a, long mod) {
		Pair p = new Pair(0, 0);
		long g = gcd(a, mod, p, mod);
		if (g != 1) {
			throw new AssertionError();
		} else {
			return mod(p.x, mod);
		}
	}
	
	private static long gcd(long a, long b, Pair p, long mod) {
		if (a == 0) {
			p.x = 0;
			p.y = 1;
			return b;
		}
		Pair p1 = new Pair(0, 0);
		long d = gcd(b % a, a, p1, mod);
		p.x = mod(p1.y - (b / a) * p1.x, mod);
		p.y = mod(p1.x, mod);
		return mod(d, mod);
	}
	
	private static long mod(long a, long mod) {
		a %= mod;
		return (a + mod) % mod;
	}
	
	private static class Pair {
		long x;
		long y;
		
		public Pair(long x, long y) {
			this.x = x;
			this.y = y;
		}
	}
	
	private static class Quad {
		int a, b, n, m;
		
		public Quad(int a, int b, int n, int m) {
			this.a = a;
			this.b = b;
			this.n = n;
			this.m = m;
		}
	}
	
}