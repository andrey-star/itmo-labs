import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class C_SufTree {
	
	public static int states = 0;
	public static int length;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s = in.readLine();
		in.close();
		length = s.length();
		
		Pair<int[], int[]> suf = buildSufArray(s);
		Node tree = buildSufTree(suf.a, suf.b);
		
		PrintWriter out = new PrintWriter(System.out);
		out.println(states + " " + (states - 1));
		printTree(tree, out);
		out.close();
	}
	
	private static Node buildSufTree(int[] suf, int[] lcp) {
		Node root = new Node(null, 0);
		Node prev = root;
		for (int i = 1; i <= length; i++) {
			prev = addSuffix(prev, length - suf[i], lcp[i]);
		}
		index(root);
		return root;
	}
	
	private static Node addSuffix(Node prev, int length, int lcp) {
		if (prev.d == 0 || prev.d == lcp) {
			Node leaf = new Node(prev, length);
			prev.ch.push(leaf);
			return leaf;
		}
		if (prev.p.d < lcp) {
			insertNode(prev, lcp);
		}
		return addSuffix(prev.p, length, lcp);
	}
	
	private static void insertNode(Node prev, int lcp) {
		Node split = new Node(prev.p, lcp);
		prev.p.ch.pop();
		prev.p.ch.push(split);
		split.ch.push(prev);
		prev.p = split;
	}
	
	private static void index(Node node) {
		if (node.index == -1) {
			node.index = states++;
		}
		for (Node child : node.ch) {
			index(child);
		}
	}
	
	private static Pair<Integer, Integer> getSubstring(Node parent, Node child) {
		int start = length - (maxDepth(child) - parent.d);
		int end = length - (maxDepth(child) - child.d);
		return new Pair<>(start, end);
	}
	
	private static int maxDepth(Node node) {
		if (node.md == -1) {
			if (node.ch.isEmpty()) {
				node.md = node.d;
			} else {
				for (Node child : node.ch) {
					node.md = Math.max(node.md, maxDepth(child));
				}
			}
		}
		return node.md;
	}
	
	private static void printRes(Node node, PrintWriter out) {
		for (Node child : node.ch) {
			Pair<Integer, Integer> positions = getSubstring(node, child);
			out.println(node.index + 1 + " " + (child.index + 1) + " " + (positions.a + 1) + " " + positions.b);
			printRes(child, out);
		}
	}
	
	private static void printTree(Node node, PrintWriter out) {
		for (Node child : node.ch) {
			Pair<Integer, Integer> positions = getSubstring(node, child);
			out.println(node.index + 1 + " -> " + (child.index + 1) + " s[" + (positions.a + 1) + ".." + positions.b +"]");
			printTree(child, out);
		}
	}
	
	private static Pair<int[], int[]> buildSufArray(String s) {
		s += "#";
		int n = s.length();
		int[] p = new int[n];
		int[] c = new int[n];
		//noinspection unchecked
		Pair<Character, Integer>[] a = new Pair[n];
		for (int i = 0; i < n; i++) {
			a[i] = new Pair<>(s.charAt(i), i);
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
		return new Pair<>(p, lcp);
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
	
	public static class Node {
		public int d; // distance in characters from root to this node
		public Node p;
		Deque<Node> ch;
		int index = -1;
		int md = -1;
		
		Node(Node parent, int depth) {
			this.p = parent;
			this.d = depth;
			ch = new ArrayDeque<>();
		}
	}
	
	private static class Pair<A, B> {
		A a;
		B b;
		
		public Pair(A a, B b) {
			this.a = a;
			this.b = b;
		}
	}
}
