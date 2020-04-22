import java.io.*;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Queue;

public class I_AhoCorasick {
	
	private static final int ALPHABET_SIZE = 26;
	private static final int MAX_STATES = (int) 2e6;
	private static int states = 1;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
//		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("test.in")));
		String t = in.readLine();
		int n = Integer.parseInt(in.readLine());
		String[] ss = new String[n];
		for (int i = 0; i < n; i++) {
			ss[i] = in.readLine();
		}
		in.close();
		
		Node[] bor = new Node[MAX_STATES];
		bor[0] = new Node(-1, -1, -1);
		
		for (String s : ss) {
			addWord(s, bor);
		}
		
		
		Queue<Integer> q = new ArrayDeque<>();
		q.add(0);
		while (!q.isEmpty()) {
			int cur = q.remove();
			if (cur != 0) {
				sufLink(cur, bor);
			}
			for (int node : bor[cur].tr) {
				if (node != -1) {
					q.add(node);
				}
			}
		}
		bor[0].suf = 0;
		int curState = 0;
		for (int i = 0; i < t.length(); i++) {
			int c = t.charAt(i) - 'a';
			curState = sufChar(curState, c, bor);
			bor[curState].visited = true;
		}
		
		for (int i = 1; i < states; i++) {
			int cur = i;
			if (bor[cur].visited) {
				cur = bor[cur].suf;
				while (cur != 0 && !bor[cur].visited) {
					bor[cur].visited = true;
					cur = bor[cur].suf;
				}
			}
		}
		boolean[] res = new boolean[n];
		for (int i = 0; i < n; i++) {
			res[i] = contains(ss[i], bor);
		}
		PrintWriter out = new PrintWriter(System.out);
		for (boolean re : res) {
			out.println(re ? "Yes" : "No");
		}
		out.close();
	}
	
	private static boolean contains(String s, Node[] bor) {
		int cur = 0;
		for (int i = 0; i < s.length(); i++) {
			int c = s.charAt(i) - 'a';
			cur = bor[cur].tr[c];
		}
		return bor[cur].visited;
	}
	
	private static void sufLink(int u, Node[] bor) {
		Node node = bor[u];
		int p = node.p;
		p = bor[p].suf;
		while (p != -1 && bor[p].tr[node.c] == -1) {
			p = bor[p].suf;
		}
		if (p == -1) {
			node.suf = 0;
		} else {
			node.suf = bor[p].tr[node.c];
		}
	}
	
	private static int sufChar(int u, int c, Node[] bor) {
		if (bor[u].sufChar[c] == -1) {
			if (bor[u].tr[c] != -1) {
				bor[u].sufChar[c] = bor[u].tr[c];
			} else if (u == 0) {
				bor[u].sufChar[c] = 0;
			} else {
				bor[u].sufChar[c] = sufChar(bor[u].suf, c, bor);
			}
		}
		return bor[u].sufChar[c];
	}
	
	private static void addWord(String s, Node[] bor) {
		int cur = 0;
		for (int i = 0; i < s.length(); i++) {
			int c = s.charAt(i) - 'a';
			if (bor[cur].tr[c] == -1) {
				bor[states] = new Node(cur, c, -1);
				bor[cur].tr[c] = states++;
			}
			cur = bor[cur].tr[c];
		}
	}
	
	private static class Node {
		int p;
		int c;
		int suf;
		int[] tr;
		int[] sufChar;
		boolean visited;
		
		public Node(int p, int c, int suf) {
			this.p = p;
			this.c = c;
			this.suf = suf;
			tr = new int[ALPHABET_SIZE];
			Arrays.fill(tr, -1);
			sufChar = new int[ALPHABET_SIZE];
			Arrays.fill(sufChar, -1);
			visited = false;
		}
		
		@Override
		public String toString() {
			return "Node{" +
					"p=" + p +
					", c=" + (char) (c + 'a') +
					", suf=" + suf +
					", tr=" + Arrays.toString(tr) +
					", visited=" + visited +
					'}';
		}
		
	}
	
}
