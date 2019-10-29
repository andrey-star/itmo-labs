import java.io.*;
import java.util.Arrays;

public class J_Hirschberg {
	
	private static int[][] pref;
	private static int[][] suf;
	
	private static String getLcs(String s, String t) {
		if (s.length() == 1) {
			if (t.contains(s)) {
				return s;
			}
			return "";
		}
		if (t.length() == 1) {
			if (s.contains(t)) {
				return t;
			}
			return "";
		}
		int n = s.length();
		int m = t.length();
		for (int[] p : pref) {
			Arrays.fill(p, 0);
		}
		for (int[] su : suf) {
			Arrays.fill(su, 0);
		}
		int l = n / 2;
		
		for (int i = 0; i < l; i++) {
			for (int j = 0; j < m + 1; j++) {
				pref[j][0] = pref[j][1];
			}
			for (int j = 1; j <= m; j++) {
				if (s.charAt(i) == t.charAt(j - 1)) {
					pref[j][1] = pref[j - 1][0] + 1;
				} else {
					pref[j][1] = Math.max(pref[j][0], pref[j - 1][1]);
				}
			}
		}
		for (int i = n - 1; i >= l; i--) {
			for (int j = 0; j < m + 2; j++) {
				suf[j][0] = suf[j][1];
			}
			for (int j = m; j >= 1; j--) {
				if (s.charAt(i) == t.charAt(j - 1)) {
					suf[j][1] = suf[j + 1][0] + 1;
				} else {
					suf[j][1] = Math.max(suf[j][0], suf[j + 1][1]);
				}
			}
		}
		
		int maxIndex = 0;
		for (int i = 0; i <= m; i++) {
			if (pref[i][1] + suf[i + 1][1] > pref[maxIndex][1] + suf[maxIndex + 1][1]) {
				maxIndex = i;
			}
		}
		return getLcs(s.substring(0, l), t.substring(0, maxIndex)) + getLcs(s.substring(l), t.substring(maxIndex));
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s1 = in.readLine();
		String s2 = in.readLine();
		pref = new int[s2.length() + 1][2];
		suf = new int[s2.length() + 2][2];
		PrintWriter out = new PrintWriter(System.out);
		out.println(getLcs(s1, s2));
		out.close();
	}
	
}