import java.io.*;
import java.util.*;

public class E_WordsLengthNka {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("problem5.in")));
		String[] line = in.readLine().trim().split(" +");
		int ns = Integer.parseInt(line[0]);
		int ms = Integer.parseInt(line[1]);
		int ks = Integer.parseInt(line[2]);
		int l = Integer.parseInt(line[3]);
		BitSet term = new BitSet(ns);
		line = in.readLine().trim().split(" +");
		for (int i = 0; i < ks; i++) {
			term.set(Integer.parseInt(line[i]) - 1);
		}
		BitSet[][] g = new BitSet[ns]['z' - 'a' + 1];
		for (int i = 0; i < ns; i++) {
			for (int j = 0; j < 'z' - 'a' + 1; j++) {
				g[i][j] = new BitSet(ns);
			}
		}
		for (int i = 0; i < ms; i++) {
			line = in.readLine().trim().split(" +");
			int a = Integer.parseInt(line[0]) - 1;
			int b = Integer.parseInt(line[1]) - 1;
			char c = line[2].charAt(0);
			g[a][c - 'a'].set(b);
		}
		List<BitSet> queue = new LinkedList<>();
		queue.add(new BitSet(ns));
		queue.get(0).set(0);
		List<BitSet> q = new LinkedList<>();
		List<BitSet2Int> deltaD = new LinkedList<>();
		q.add(queue.get(0));
		while (!queue.isEmpty()) {
			BitSet pd = queue.remove(0);
			for (int c = 0; c < 'z' - 'a' + 1; c++) {
				BitSet qd = new BitSet(ns);
				for (int i = pd.nextSetBit(0); i >= 0; i = pd.nextSetBit(i + 1)) {
					qd.or(g[i][c]);
				}
				if (!qd.isEmpty()) {
					deltaD.add(new BitSet2Int(pd, qd, c));
					if (!q.contains(qd)) {
						queue.add(qd);
						q.add(qd);
					}
				}
			}
		}
		int n = q.size();
		BitSet newTerm = new BitSet(n);
		for (int i = 0; i < n; i++) {
			BitSet qI = new BitSet(ns);
			for (int j = q.get(i).nextSetBit(0); j >= 0; j = q.get(i).nextSetBit(j + 1)) {
				qI.set(j);
			}
			qI.and(term);
			if (qI.length() != 0) {
				newTerm.set(i);
			}
		}
		int[][] gNew = new int[n][26];
		for (int i = 0; i < gNew.length; i++) {
			for (int j = 0; j < gNew[i].length; j++) {
				gNew[i][j] = -1;
			}
		}
		for (BitSet2Int edge : deltaD) {
			BitSet a = edge.a;
			BitSet b = edge.b;
			int c = edge.letter;
			gNew[q.indexOf(a)][c] = q.indexOf(b);
		}
		int[][] dp = new int[l + 1][n];
		dp[0][0] = 1;
		for (int i = 0; i < l; i++) {
			for (int j = 0; j < n; j++) {
				for (char c = 'a'; c <= 'z'; c++) {
					if (gNew[j][c - 'a'] != -1) {
						dp[i + 1][gNew[j][c - 'a']] += dp[i][j];
						dp[i + 1][gNew[j][c - 'a']] %= (int) 1e9 + 7;
					}
				}
			}
		}
		int sum = 0;
		for (int i = 0; i < n; i++) {
			if (newTerm.get(i)) {
				sum += dp[l][i];
				sum %= (int) 1e9 + 7;
			}
		}
		
		PrintWriter out = new PrintWriter(new File("problem5.out"));
		out.println(sum);
		out.close();
	}
	
	static class BitSet2Int {
		BitSet a;
		BitSet b;
		int letter;
		
		BitSet2Int(BitSet a, BitSet b, int index) {
			this.a = a;
			this.b = b;
			this.letter = index;
		}
		
	}
	
}