import java.io.*;
import java.util.*;

public class D_Nfc {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("nfc.in")));
		String[] line = in.readLine().trim().split(" +");
		int rulesNumTotal = Integer.parseInt(line[0]);
		int start = line[1].charAt(0) - 'A';
		HashMap<Integer, Set<Pair>> toNonTerm = new HashMap<>();
		HashMap<Integer, Set<Character>> toTerm = new HashMap<>();
		for (int i = 0; i < rulesNumTotal; i++) {
			line = in.readLine().trim().split(" +");
			int left = line[0].charAt(0) - 'A';
			String right = line[2];
			if (Character.isLowerCase(right.charAt(0))) {
				toTerm.putIfAbsent(left, new HashSet<>());
				toTerm.get(left).add(right.charAt(0));
			} else {
				int first = right.charAt(0) - 'A';
				int second = right.charAt(1) - 'A';
				toNonTerm.putIfAbsent(left, new HashSet<>());
				toNonTerm.get(left).add(new Pair(first, second));
			}
		}
		String w = in.readLine();
		int n = w.length();
		int l = 'z' - 'a' + 1;
		int[][][] d = new int[l][n][n];
		for (int i = 0; i < w.length(); i++) {
			char c = w.charAt(i);
			for (int left : toTerm.keySet()) {
				for (char right : toTerm.get(left)) {
					if (right == c) {
						d[left][i][i] = 1;
						break;
					}
				}
			}
		}
		for (int m = 1; m < n; m++) {
			for (int i = 0; i + m < n; i++) {
				int j = i + m;
				for (int a = 0; a < l; a++) {
					if (toNonTerm.containsKey(a)) {
						int res = 0;
						for (Pair right : toNonTerm.get(a)) {
							int b = right.a;
							int c = right.b;
							for (int k = i; k < j; k++) {
								res += ((long) d[b][i][k] * d[c][k + 1][j]) % ((int) 1e9 + 7);
								res %= (int) 1e9 + 7;
							}
						}
						d[a][i][j] = res;
					}
				}
			}
		}
		PrintWriter out = new PrintWriter(new File("nfc.out"));
		out.println(d[start][0][n - 1]);
		out.close();
	}
	
	private static class Pair {
		final int a;
		final int b;
		
		Pair(int a, int b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public boolean equals(Object obj) {
			Pair f = ((Pair) obj);
			return a == f.a && b == f.b;
		}
		
		@Override
		public int hashCode() {
			return (a * 0x1f1f1f1f) ^ b;
		}
		
		@Override
		public String toString() {
			return "IntPair{" +
					"left=" + a +
					", right=" + b +
					'}';
		}
	}
	
}
