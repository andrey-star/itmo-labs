import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

public class E_SubstringOneMismatch {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String p = in.readLine();
		String t = in.readLine();
		in.close();
		String pRev = new StringBuilder(p).reverse().toString();
		String tRev = new StringBuilder(t).reverse().toString();
		
		String s = p + '#' + t;
		String sRev = pRev + "#" + tRev;
		int[] z = z(s);
		int[] zRev = z(sRev);
		List<Integer> res = new ArrayList<>();
		for (int i = p.length() + 1; i <= s.length() - p.length(); i++) {
			if (z[i] == p.length()) {
				res.add(i - p.length());
			} else if (z[i] + zRev[t.length() - i + p.length() + 2] == p.length() - 1) {
				res.add(i - p.length());
			}
		}
		PrintWriter out = new PrintWriter(System.out);
		out.println(res.size());
		for (int re : res) {
			out.print(re + " ");
		}
		out.close();
	}
	
	private static int[] z(String s) {
		int n = s.length();
		int[] z = new int[n];
		int l = 0;
		for (int i = 1; i < n; i++) {
			if (i <= l + z[l] - 1) {
				int j = i - l;
				z[i] = Math.min(z[l] - j, z[j]);
			}
			while (i + z[i] < n && s.charAt(z[i]) == s.charAt(i + z[i])) {
				z[i]++;
				l = i;
			}
		}
		return z;
	}
	
}
