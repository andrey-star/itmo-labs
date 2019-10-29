import java.io.*;
import java.util.*;

public class E_Ksg {
	
	private static boolean onlyLowerCase(String s) {
		if (s.length() == 0) {
			return false;
		}
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (!Character.isLowerCase(c)) {
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("epsilon.in")));
		String[] line = in.readLine().trim().split(" +");
		int rulesNum = Integer.parseInt(line[0]);
		int start = line[1].charAt(0) - 'A';
		List<Pair> rules = new ArrayList<>();
		for (int i = 0; i < rulesNum; i++) {
			line = in.readLine().trim().split(" +");
			String left = line[0];
			String right = (line.length == 3 ? line[2] : "");
			rules.add(new Pair(left.charAt(0), right));
		}
		String w = in.readLine();
		int n = w.length();
		int l = 'z' - 'a' + 1;
		int[][][] d = new int[l][n][n];
		for (int i = 0; i < w.length(); i++) {
			char c = w.charAt(i);
//			for (int left : toTerm.keySet()) {
//				for (char right : toTerm.get(left)) {
//					if (right == c) {
//						d[left][i][i] = 1;
//					}
//				}
//			}
		}
		for (int m = 1; m < n; m++) {
			for (int i = 0; i + m < n; i++) {
				int j = i + m;
				for (int a = 0; a < l; a++) {
//					if (toNonTerm.containsKey(a)) {
//						int res = 0;
//						for (Pair right : toNonTerm.get(a)) {
//							int b = right.a;
//							int c = right.b;
//							for (int k = i; k < j; k++) {
//								res += ((long) d[b][i][k] * d[c][k + 1][j]) % ((int) 1e9 + 7);
//								res %= (int) 1e9 + 7;
//							}
//						}
//						d[a][i][j] = res;
//					}
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("nfc.out"));
		out.println(d[start][0][n - 1]);
		out.close();
	}
	
	private static class Pair {
		char a;
		String b;
		
		Pair(char a, String b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public String toString() {
			return "Pair{" +
					"left=" + a +
					", right='" + b + '\'' +
					'}';
		}
	}
	
}
