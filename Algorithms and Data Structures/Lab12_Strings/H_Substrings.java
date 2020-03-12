import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class H_Substrings {
	
	private static final int X = 31;
	
	private static long[] xPow;
	private static long[] ph;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String s = in.readLine();
		int qu = Integer.parseInt(in.readLine());
		Query[] queries = new Query[qu];
		String[] line;
		for (int i = 0; i < qu; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			int x = Integer.parseInt(line[2]) - 1;
			int y = Integer.parseInt(line[3]) - 1;
			queries[i] = new Query(a, b, x, y);
		}
		in.close();
		
		int n = s.length();
		xPow(n);
		ph(s);
		
		PrintWriter out = new PrintWriter(System.out);
		for (Query query : queries) {
			int a = query.a;
			int b = query.b;
			int x = query.x;
			int y = query.y;
			if (b - a != y - x
					|| hash(a, b + 1) != hash(x, y + 1)
					|| !equals(s, a, b + 1, x, y + 1)) {
				out.println("No");
			} else {
				out.println("Yes");
			}
		}
		out.close();
	}
	
	private static long hash(int from, int to) {
		return ph[to] - ph[from] * xPow[to - from];
	}
	
	private static boolean equals(String s, int a, int b, int x, int y) {
		for (int i = 0; i < b - a; i++) {
			if (s.charAt(i + a) != s.charAt(i + x)) {
				return false;
			}
		}
		return true;
	}
	
	private static void ph(String s) {
		int n = s.length();
		ph = new long[n + 1];
		ph[0] = 0;
		for (int i = 1; i <= n; i++) {
			ph[i] = ph[i - 1] * X + s.charAt(i - 1);
		}
	}
	
	private static void xPow(int n) {
		xPow = new long[n + 1];
		xPow[0] = 1;
		for (int i = 1; i <= n; i++) {
			xPow[i] = xPow[i - 1] * X;
		}
	}
	
	private static class Query {
		
		int a, b;
		int x, y;
		
		public Query(int a, int b, int x, int y) {
			this.a = a;
			this.b = b;
			this.x = x;
			this.y = y;
		}
	}
	
}
