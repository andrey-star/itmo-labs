import java.io.*;
import java.util.Random;

public class C_MoveToFront {
	
	static class Node {
		long priority;
		int size;
		int value;
		Node left, right, parent;
		
		Node(int value, long priority) {
			this.value = value;
			this.priority = priority;
			this.size = 1;
		}
		
		@Override
		public String toString() {
			return "v=" + value + ", p=" + priority +
					", s=" + size;
		}
	}
	
	class ImplicitTreap {
		
		Node root;
		private final Random random = new Random();
		
		public Node[] split(int amount) {
			return split(root, amount);
		}
		
		private Node[] split(Node t, int value) {
			Node[] res = new Node[2];
			if (t == null) {
				return res;
			}
			int l = size(t.left);
			if (value > l) {
				Node[] innerSplit = split(t.right, value - l - 1);
				t.right = innerSplit[0];
				if (innerSplit[0] != null) {
					innerSplit[0].parent = t;
				}
				res[0] = t;
				res[1] = innerSplit[1];
			} else {
				Node[] innerSplit = split(t.left, value);
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
			updateSize(res[0]);
			updateSize(res[1]);
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
				updateSize(a);
				return a;
			} else {
				Node merge = merge(a, b.left);
				b.left = merge;
				if (merge != null) {
					merge.parent = b;
				}
				updateSize(b);
				return b;
			}
		}
		
		void insert(int value, long priority) {
			Node[] split = split(root, value);
			root = merge(merge(split[0], new Node(value, priority)), split[1]);
		}
		
		public void insert(int value) {
			insert(value, random.nextLong());
		}
		
		public void remove(int key) {
			Node[] spl = split(root, key);
			Node el = spl[1];
			if (el != null) {
				while (el.left != null) {
					el.size--;
					el = el.left;
				}
				if (el.parent != null) {
					el.parent.left = el.right;
					if (el.right != null) {
						el.right.parent = el.parent;
					}
				} else {
					el = el.right;
					if (el != null) {
						el.parent = null;
					}
					spl[1] = el;
				}
			}
			root = merge(spl[0], spl[1]);
			updateSize(root);
		}
		
		public String preOrder() {
			StringBuilder sb = new StringBuilder();
			preOrder(root, sb);
			return sb.toString();
		}
		
		private String preOrder(Node node) {
			StringBuilder sb = new StringBuilder();
			preOrder(node, sb);
			return sb.toString();
		}
		
		private void preOrder(Node a, StringBuilder sb) {
			if (a == null) {
				return;
			}
			preOrder(a.left, sb);
//		sb.append("(")
//				.append(a)
//				.append(")")
//				.append(" ")
//				.append(whichChild(a))
//				.append(" of ")
//				.append(a.parent == null ? "null" : a.parent.value)
//				.append("\n");
			sb.append(a.value).append(" ");
			preOrder(a.right, sb);
		}
		
		private String whichChild(Node node) {
			if (node.parent == null) {
				return "null";
			} else if (node.parent.left == null || node.parent.left.value != node.value
					|| node.parent.left.priority != node.priority) {
				return "right";
			} else {
				return "left";
			}
		}
		
		public void moveToFront(int l, int r) {
			if (l > r) {
				return;
			}
			Node[] byR = split(r);
			Node lessThanOrEqualRight = byR[0];
			Node greaterThanRight = byR[1];
			
			Node[] byL = split(lessThanOrEqualRight, l - 1);
			Node lessThanLeft = byL[0];
			Node lToR = byL[1];
			merge(lToR, merge(lessThanLeft, greaterThanRight));
		}
		
		private int size(Node node) {
			return node == null ? 0 : node.size;
		}
		
		private void updateSize(Node node) {
			if (node != null) {
				node.size = 1 + size(node.left) + size(node.right);
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		new C_MoveToFront().run();
	}
	
	public void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		ImplicitTreap t = new ImplicitTreap();
		for (int i = 0; i < n; i++) {
			t.insert(i + 1);
		}
		for (int i = 0; i < m; i++) {
			line = in.readLine().trim().split(" +");
			int l = Integer.parseInt(line[0]);
			int r = Integer.parseInt(line[1]);
			t.moveToFront(l, r);
		}
		PrintWriter out = new PrintWriter(System.out);
		out.println(t.preOrder());
		out.close();
		in.close();
	}
}