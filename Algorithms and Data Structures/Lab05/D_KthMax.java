import java.io.*;
import java.util.*;

public class D_KthMax {
	
	class Node {
		int key;
		long priority;
		int size;
		Node left, right, parent;
		
		Node(int key, long priority) {
			this.key = key;
			this.priority = priority;
			this.size = 1;
		}
		
		@Override
		public String toString() {
			return "k=" + key +
					", p=" + priority +
					", s=" + size;
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
			updateSize(res[0]);
			updateSize(res[1]);
			return res;
		}
		
		public Node merge(Node a, Node b) {
			if (a == null) {
				updateSize(b);
				return b;
			}
			if (b == null) {
				updateSize(a);
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
		
		void insert(int key, long priority) {
			Node[] split = split(root, key);
			root = merge(merge(split[0], new Node(key, priority)), split[1]);
		}
		
		public void insert(int key) {
			long l = random.nextLong();
			insert(key, l);
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
		
		private int size(Node node) {
			return node == null ? 0 : node.size;
		}
		
		private void updateSize(Node node) {
			if (node != null) {
				node.size = 1 + size(node.left) + size(node.right);
			}
		}
		
	}
	
	
	private int kthMax(Treap t, int k) {
		return kthMax(t.root, k);
	}
	
	private int kthMax(Node node, int k) {
		int skipLeft = 1 + (node.right != null ? node.right.size : 0);
		if (k - skipLeft < 0) {
			return kthMax(node.right, k);
		} else if (k - skipLeft > 0) {
			return kthMax(node.left, k - skipLeft);
		}
		return node.key;
	}
	
	public static void main(String[] args) throws IOException {
		D_KthMax m = new D_KthMax();
		m.run();
	}
	
	private void test() {
		Treap t = new Treap();
		t.insert(1);
		t.insert(2);
		t.insert(3);
		t.insert(4);
		t.insert(5);
		t.insert(6);
		t.insert(7);
		t.insert(8);
		t.insert(9);
		t.remove(5);
		t.remove(3);
		t.insert(3);
	}
	
	private void genTest() throws FileNotFoundException {
		PrintWriter out = new PrintWriter(new File("input.txt"));
		Random random = new Random();
		int n = 1000;
		out.println(1000);
		List<Integer> keys = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			int op = random.nextInt(3);
			if (op == 0 || keys.size() == 0) {
				int ind = random.nextInt(1000);
				while (keys.contains(ind)) {
					ind = random.nextInt(1000);
				}
				keys.add(ind);
				out.println("+1 " + ind);
			} else if (op == 1) {
				int k = 1 + random.nextInt(keys.size());
				out.println("0 " + k);
			} else if (op == 2) {
				int ind = random.nextInt(keys.size());
				out.println("-1 " + keys.get(ind));
				keys.remove(ind);
			}
		}
		out.close();
	}
	
	private void run() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		PrintWriter out = new PrintWriter(System.out);
		int n = Integer.parseInt(in.readLine());
		Treap t = new Treap();
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			String cmd = line[0];
			int k = Integer.parseInt(line[1]);
			switch (cmd) {
				case "+1":
				case "1":
					t.insert(k);
					break;
				case "-1":
					t.remove(k);
					break;
				case "0":
					out.println(kthMax(t, k));
					break;
			}
		}
		in.close();
		out.close();
	}
}
