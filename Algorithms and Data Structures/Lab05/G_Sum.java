import java.io.*;
import java.util.*;

public class G_Sum {
	
	class Node {
		int key;
		long priority;
		long sum;
		Node left, right, parent;
		
		Node(int key, long priority) {
			this.key = key;
			this.priority = priority;
			this.sum = key;
		}
		
		@Override
		public String toString() {
			return "k=" + key +
					", p=" + priority +
					", s=" + sum;
		}
	}
	
	class Treap {
		
		Node root;
		private Random random = new Random();
		
		public Node[] split(int key) {
			return split(root, key);
		}
		
		private Node[] split(Node t, int key) {
			Node[] res = new Node[2];
			if (t == null) {
				return res;
			} else if (key > t.key) {
				Node[] innerSplit = split(t.right, key);
				t.right = innerSplit[0];
				if (innerSplit[0] != null) {
					innerSplit[0].parent = t;
				}
				res[0] = t;
				res[1] = innerSplit[1];
			} else {
				Node[] innerSplit = split(t.left, key);
				t.left = innerSplit[1];
				if (innerSplit[1] != null) {
					innerSplit[1].parent = t;
				}
				res[0] = innerSplit[0];
				res[1] = t;
			}
			if (res[0] != null) {
				res[0].parent = null;
			}
			if (res[1] != null) {
				res[1].parent = null;
			}
			updateSum(res[0]);
			updateSum(res[1]);
			return res;
		}
		
		public Node merge(Node a, Node b) {
			if (a == null) {
				return b;
			}
			if (b == null) {
				return a;
			}
			if (a.priority > b.priority) {
				Node merge = merge(a.right, b);
				a.right = merge;
				if (merge != null) {
					merge.parent = a;
				}
				updateSum(a);
				return a;
			} else {
				Node merge = merge(a, b.left);
				b.left = merge;
				if (merge != null) {
					merge.parent = b;
				}
				updateSum(b);
				return b;
			}
		}
		
		void insert(int key, long priority) {
			if (contains(key)) {
				return;
			}
			Node[] split = split(root, key);
			root = merge(merge(split[0], new Node(key, priority)), split[1]);
		}
		
		private boolean contains(int key) {
			Node check = root;
			while (check != null) {
				if (check.key > key) {
					check = check.left;
				} else if (check.key < key) {
					check = check.right;
				} else {
					return true;
				}
			}
			return false;
		}
		
		public void insert(int key) {
			long l = random.nextLong();
			insert(key, l);
		}
		
		public String inOrder() {
			return inOrder(root);
		}
		
		private String inOrder(Node node) {
			StringBuilder sb = new StringBuilder();
			inOrder(node, sb);
			return sb.toString();
		}
		
		private void inOrder(Node a, StringBuilder sb) {
			if (a == null) {
				return;
			}
			sb.append("(")
					.append(a)
					.append(")")
					.append(" ")
					.append(whichChild(a))
					.append(" of ")
					.append(a.parent == null ? "null" : a.parent.key)
					.append("\n");
			inOrder(a.left, sb);
			inOrder(a.right, sb);
		}
		
		private String whichChild(Node node) {
			if (node.parent == null) {
				return "null";
			} else if (node.parent.left == null || node.parent.left.key != node.key
					|| node.parent.left.priority != node.priority) {
				return "right";
			} else {
				return "left";
			}
		}
		
		private long getSum(Node node) {
			return node == null ? 0 : node.sum;
		}
		
		private void updateSum(Node node) {
			if (node != null) {
				node.sum = node.key + getSum(node.left) + getSum(node.right);
			}
		}
		
		public long sum(int l, int r) {
			if (l > r) {
				return 0;
			}
			Node[] byR = split(r + 1);
			Node lessThanRight = byR[0];
			Node greaterThanRight = byR[1];
			
			Node[] byL = split(lessThanRight, l);
			Node lessThanLeft = byL[0];
			Node lToR = byL[1];
			
			long res = getSum(lToR);
			lessThanRight = merge(byL[0], byL[1]);
			merge(lessThanRight, greaterThanRight);
			return res;
		}
		
	}
	
	public static void main(String[] args) throws IOException {
		G_Sum m = new G_Sum();
		m.run();
//		m.test();
	}
	
	private void test() {
		Treap t = new Treap();
		t.insert(8);
		System.out.println(t.inOrder());
		t.insert(8);
		System.out.println(t.inOrder());
//		Node[] s = t.split(0);
//		System.out.println(t.inOrder(s[0]));
//		System.out.println(t.inOrder(s[1]));
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		PrintWriter out = new PrintWriter(System.out);
		int n = Integer.parseInt(in.readLine());
		Treap t = new Treap();
		String lastOp = "+";
		long lastRes = 0;
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			int add = Integer.parseInt(line[1]);
			if (line[0].equals("+")) {
				if (lastOp.equals("+")) {
					t.insert(add);
				} else {
					t.insert((int) ((add + lastRes) % 1e9));
					lastOp = "+";
				}
			} else {
				int l = Integer.parseInt(line[1]);
				int r = Integer.parseInt(line[2]);
				lastRes = t.sum(l, r);
				out.println(lastRes);
				lastOp = "?";
			}
		}
		in.close();
		out.close();
	}
}
