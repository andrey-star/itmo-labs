import java.util.*;

public class Treap {
	
	static class Node {
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
	
	Node root;
	private final Random random = new Random();
	
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
	
	void insert(int key, long priority) {
		Node[] split = split(root, key);
		root = merge(merge(split[0], new Node(key, priority)), split[1]);
	}
	
	public void insert(int key) {
		insert(key, random.nextLong());
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
		StringBuilder sb = new StringBuilder();
		inOrder(root, sb);
		return sb.toString();
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
	
	public static void main(String[] args) {
	
	}
	
	public static void genTest() {
		Random random = new Random();
		List<Integer> keys = new ArrayList<>();
		List<Integer> pr = new ArrayList<>();
		for (int i = 0; i < 10; i++) {
			keys.add(i);
		}
		while (pr.size() < 10) {
			int add = random.nextInt(100);
			if (!pr.contains(add)) {
				pr.add(add);
			}
		}
		for (int i = 0; i < 10; i++) {
			System.out.println("t.add(" + keys.get(i) + ", " + pr.get(i) + ");");
		}
	}
}
