import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Random;

public class F_Keys {
	
	class Node {
		long priority;
		int size;
		int value;
		int notZeroRight;
		Node left, right, parent;
		
		Node(int value, long priority) {
			this.value = value;
			this.priority = priority;
			this.size = 1;
			this.notZeroRight = value == 0 ? 0 : 1;
		}
		
		@Override
		public String toString() {
			return "v=" + value + ", p=" + priority +
					", s=" + size;
		}
	}
	
	class ImplicitTreap {
		
		Node root;
		private Random random = new Random(2);
		
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
			updateValues(res[0]);
			updateValues(res[1]);
			return res;
		}
		
		public Node merge(Node a, Node b) {
			if (a == null) {
				updateValues(b);
				return b;
			}
			if (b == null) {
				updateValues(a);
				return a;
			}
			if (a.priority > b.priority) {
				Node merge = merge(a.right, b);
				a.right = merge;
				if (merge != null) {
					merge.parent = a;
				}
				updateValues(a);
				return a;
			} else {
				Node merge = merge(a, b.left);
				b.left = merge;
				if (merge != null) {
					merge.parent = b;
				}
				updateValues(b);
				return b;
			}
		}
		
		private void add(int value, long priority) {
			root = merge(root, new Node(value, priority));
		}
		
		public void add(int value) {
			add(value, random.nextLong());
		}
		
		public void set(int value, int index) {
			if (index < 1) {
				return;
			}
			Node[] spl = split(index - 1);
			Node right = spl[1];
			int len = right.notZeroRight;
			Node[] rSpl = split(right, len);
			rSpl[1] = removeFirst(rSpl[1]);
			root = merge(merge(spl[0], new Node(value, random.nextLong())), merge(rSpl[0], rSpl[1]));
		}
		
		private Node removeFirst(Node start) {
			return split(start, 1)[1];
		}
		
		public String preOrder() {
			return preOrder(root);
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
			sb.append(a.value).append(" ");
			preOrder(a.right, sb);
		}
		
		private int size(Node node) {
			return node == null ? 0 : node.size;
		}
		
		private void updateValues(Node node) {
			if (node != null) {
				node.size = 1 + size(node.left) + size(node.right);
				
				int notZeroRight = 0;
				if (node.left != null) {
					notZeroRight += node.left.notZeroRight;
					if (node.left.notZeroRight < node.left.size) {
						node.notZeroRight = notZeroRight;
						return;
					}
				}
				
				if (node.value != 0) {
					notZeroRight += 1;
					if (node.right == null) {
						node.notZeroRight = notZeroRight;
						return;
					}
				} else {
					node.notZeroRight = notZeroRight;
					return;
				}
				
				notZeroRight += node.right.notZeroRight;
				node.notZeroRight = notZeroRight;
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		new F_Keys().run();
	}
	
	public void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		
		int n = Integer.parseInt(line[0]);
		int m = Integer.parseInt(line[1]);
		ImplicitTreap t = new ImplicitTreap();
		for (int i = 0; i < m; i++) {
			t.add(0);
		}
		line = in.readLine().trim().split(" +");
		in.close();
		
		for (int i = 0; i < n; i++) {
			t.set(i + 1, Integer.parseInt(line[i]));
		}
		
		String[] res = t.preOrder().trim().split(" +");
		int lastNotZero = 0;
		for (int i = 0; i < res.length; i++) {
			if (!res[i].equals("0")) {
				lastNotZero = i + 1;
			}
		}
		PrintWriter out = new PrintWriter(System.out);
		out.println(lastNotZero);
		for (int i = 0; i < lastNotZero; i++) {
			out.print(res[i] + " ");
		}
		out.close();
	}
}