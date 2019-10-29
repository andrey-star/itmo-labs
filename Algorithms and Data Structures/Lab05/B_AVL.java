import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.NavigableSet;
import java.util.Random;
import java.util.TreeSet;

public class B_AVL {
	
	static class Node {
		Node left;
		Node right;
		Node parent;
		int value;
		int diff;
		
		Node(Node left, Node right, Node parent, int value) {
			this.left = left;
			this.right = right;
			this.parent = parent;
			this.value = value;
			diff = 0;
		}
	}
	
	static class AVLTree {
		
		private Node root;
		
		private void rotateLeft(Node a) {
			String whichChild = whichChild(a);
			Node b = a.right;
			a.right = b.left;
			if (b.left != null) {
				b.left.parent = a;
			}
			b.left = a;
			b.parent = a.parent;
			a.parent = b;
			if (whichChild.equals("right")) {
				b.parent.right = b;
			} else if (whichChild.equals("left")) {
				b.parent.left = b;
			} else {
				root = b;
			}
		}
		
		private void rotateRight(Node a) {
			String whichChild = whichChild(a);
			Node b = a.left;
			a.left = b.right;
			if (b.right != null) {
				b.right.parent = a;
			}
			b.right = a;
			b.parent = a.parent;
			a.parent = b;
			if (whichChild.equals("right")) {
				b.parent.right = b;
			} else if (whichChild.equals("left")) {
				b.parent.left = b;
			} else {
				root = b;
			}
		}
		
		private void bigRotateLeft(Node a) {
			rotateRight(a.right);
			rotateLeft(a);
		}
		
		private void bigRotateRight(Node a) {
			rotateLeft(a.left);
			rotateRight(a);
		}
		
		private void balance(Node a) {
			if (a.left == null && a.right == null) {
				return;
			}
			
			if (a.left != null) {
				Node b = a.left;
				if (a.diff == 2) {
					if (b.diff == 0) {
						rotateRight(a);
						a.diff = 1;
						b.diff = -1;
					} else if (b.diff == 1) {
						rotateRight(a);
						a.diff = 0;
						b.diff = 0;
					} else if (b.diff == -1 && b.right != null) {
						Node c = b.right;
						if (c.diff == -1) {
							bigRotateRight(a);
							a.diff = 0;
							b.diff = 1;
							c.diff = 0;
						} else if (c.diff == 0) {
							bigRotateRight(a);
							a.diff = 0;
							b.diff = 0;
							c.diff = 0;
						} else if (c.diff == 1) {
							bigRotateRight(a);
							a.diff = -1;
							b.diff = 0;
							c.diff = 0;
						}
					}
				}
			}
			
			if (a.right != null) {
				Node b = a.right;
				if (a.diff == -2) {
					if (b.diff == 0) {
						rotateLeft(a);
						a.diff = -1;
						b.diff = 1;
					} else if (b.diff == -1) {
						rotateLeft(a);
						a.diff = 0;
						b.diff = 0;
					} else if (b.diff == 1 && b.left != null) {
						Node c = b.left;
						if (c.diff == 1) {
							bigRotateLeft(a);
							a.diff = 0;
							b.diff = -1;
							c.diff = 0;
						} else if (c.diff == 0) {
							bigRotateLeft(a);
							a.diff = 0;
							b.diff = 0;
							c.diff = 0;
						} else if (c.diff == -1) {
							bigRotateLeft(a);
							a.diff = 1;
							b.diff = 0;
							c.diff = 0;
						}
					}
				}
			}
		}
		
		public void insert(int value) {
			insertFromNode(root, value);
		}
		
		public void delete(int value) {
			deleteNode(find(value));
		}
		
		private void deleteNode(Node a) {
			if (a != null) {
				if (hasChildren(a)) {
					if (a.right != null) {
						Node next = next(a);
						a.value = next.value;
						deleteNode(next);
						return;
					} else {
						Node prev = prev(a);
						a.value = prev.value;
						deleteNode(prev);
						return;
					}
				}
				String whichChild = whichChild(a);
				if (whichChild.equals("null")) {
					root = null;
				} else if (whichChild.equals("right")) {
					a.parent.right = null;
					fixDeleteNode(a.parent, "right");
					a = null;
				} else {
					a.parent.left = null;
					fixDeleteNode(a.parent, "left");
					a = null;
				}
			}
		}
		
		private void insertFromNode(Node a, int value) {
			if (a == null) {
				root = new Node(null, null, null, value);
				return;
			}
			while (true) {
				if (a.value > value) { // go left
					if (a.left == null) {
						a.left = new Node(null, null, a, value);
						fixInsert(a.left);
						return;
					} else {
						a = a.left;
					}
				} else if (a.value < value) {
					if (a.right == null) {
						a.right = new Node(null, null, a, value);
						fixInsert(a.right);
						return;
					} else {
						a = a.right;
					}
				} else {
					return;
				}
			}
		}
		
		public boolean exists(int value) {
			return find(value) != null;
		}
		
		private Node findFromRoot(Node root, int value) {
			if (root != null) {
				if (root.value == value) {
					return root;
				}
				if (root.value > value) {
					return findFromRoot(root.left, value);
				} else {
					return findFromRoot(root.right, value);
				}
			}
			return null;
		}
		
		private Node find(int value) {
			return findFromRoot(root, value);
		}
		
		private void fixDeleteNode(Node a, String deletedChild) {
			if (deletedChild.equals("left")) {
				a.diff--;
			} else {
				a.diff++;
			}
			if (a.diff == 0) {
				fixDeleteParent(a);
				return;
			}
			if (a.diff == 1 || a.diff == -1) {
				return;
			}
			if (a.diff == 2 || a.diff == -2) {
				balance(a);
				if (a.diff == 0) {
					fixDeleteParent(a);
				}
			}
		}
		
		private void fixDeleteParent(Node a) {
			String whichChild = whichChild(a);
			if (whichChild.equals("null")) {
				return;
			}
			Node p = a.parent;
			if (whichChild.equals("left")) {
				p.diff--;
			} else {
				p.diff++;
			}
			if (p.diff == 0) {
				fixDeleteParent(p);
				return;
			}
			if (p.diff == 1 || p.diff == -1) {
				return;
			}
			if (p.diff == 2 || p.diff == -2) {
				balance(p);
				if (p.diff == 0) {
					fixDeleteParent(p);
				}
			}
		}
		
		private void fixInsert(Node a) {
			String whichChild = whichChild(a);
			if (whichChild.equals("null")) {
				return;
			}
			Node p = a.parent;
			if (whichChild.equals("left")) {
				p.diff++;
			} else {
				p.diff--;
			}
			if (p.diff == 0) {
				return;
			}
			if (p.diff == 1 || p.diff == -1) {
				fixInsert(p);
				return;
			}
			if (p.diff == 2 || p.diff == -2) {
				balance(p);
				if (p.diff == 1 || p.diff == -1) {
					fixInsert(p);
				}
			}
		}
		
		public String next(int value) {
			// find
			Node node = root;
			if (node == null) {
				return "none";
			}
			while (node.value != value) {
				if (node.value > value) {
					if (node.left == null) {
						return "" + node.value;
					} else {
						node = node.left;
					}
				} else {
					if (node.right == null) {
						break;
					} else {
						node = node.right;
					}
				}
			}
			Node res = next(node);
			return res == null ? "none" : "" + res.value;
		}
		
		private Node next(Node node) {
			if (node.right != null) {
				node = node.right;
				while (node.left != null) {
					node = node.left;
				}
				return node;
			} else {
				Node y = node.parent;
				while (y != null && whichChild(node).equals("right")) {
					node = y;
					y = y.parent;
				}
				return y;
			}
		}
		
		public String prev(int value) {
			// find
			Node node = root;
			if (node == null) {
				return "none";
			}
			while (node.value != value) {
				if (node.value > value) {
					if (node.left == null) {
						break;
					} else {
						node = node.left;
					}
				} else {
					if (node.right == null) {
						return "" + node.value;
					} else {
						node = node.right;
					}
				}
			}
			Node res = prev(node);
			return res == null ? "none" : "" + res.value;
		}
		
		private Node prev(Node node) {
			if (node.left != null) {
				node = node.left;
				while (node.right != null) {
					node = node.right;
				}
				return node;
			} else {
				Node y = node.parent;
				while (y != null && whichChild(node).equals("left")) {
					node = y;
					y = y.parent;
				}
				return y;
			}
		}
		
		private boolean hasChildren(Node node) {
			return node.left != null || node.right != null;
		}
		
		private String whichChild(Node node) {
			if (node.parent == null) {
				return "null";
			} else if (node.parent.left == null || node.parent.left.value != node.value) {
				return "right";
			} else {
				return "left";
			}
		}
		
		public String inOrder() {
			trav = new StringBuilder();
			inOrder(root);
			return trav.toString();
		}
		
		private void inOrder(Node a) {
			if (a == null) {
				return;
			}
			trav.append(a.value)
					.append(" ")
					.append(whichChild(a))
					.append(" of ")
					.append(a.parent == null ? "null" : a.parent.value)
					.append("\n");
			inOrder(a.left);
			inOrder(a.right);
		}
	}
	
	static StringBuilder trav;
	
	public static void main(String[] args) throws IOException {
		solve();
//		stress();
//		test();
	}
	
	public static void test() {
		AVLTree t = new AVLTree();
		t.insert(14);
		t.insert(8);
		t.insert(1);
		t.insert(2);
		t.insert(0);
		t.delete(14);
		t.insert(10);
		t.insert(11);
		t.insert(5);
		t.insert(14);
		t.delete(1);
		t.prev(8);
		System.out.println(t.inOrder());
	}
	
	public static void stress() {
		AVLTree t = new AVLTree();
		TreeSet<Integer> s = new TreeSet<>();
		Random random = new Random();
		StringBuilder ops = new StringBuilder();
		long start = System.currentTimeMillis();
		mark: while (System.currentTimeMillis() - start < 5000) {
			int op = random.nextInt(5);
			int num = random.nextInt(15);
			t.inOrder();
			switch (op) {
				case 0:
					ops.append("t.add").append("(").append(num).append(");").append("\n");
					t.insert(num);
					s.add(num);
					break;
				case 1:
					ops.append("t.delete").append("(").append(num).append(");").append("\n");
					t.delete(num);
					s.remove(num);
					break;
				case 2:
					ops.append("t.exists").append("(").append(num).append(");").append("\n");
					if (t.exists(num) != s.contains(num)) {
//						System.out.println("exists: expected " + s.contains(num) + " found " + t.exists(num));
						break mark;
					}
					break;
				case 3:
					ops.append("t.next").append("(").append(num).append(");").append("\n");
					boolean ok = true;
					String expected = "";
					for (int i : s) {
						if (i > num) {
							if (!t.next(num).equals("" + i)) {
								ok = false;
								expected = "" + i;
							}
							break;
						}
					}
					if (!ok) {
						System.out.println("next for " + num + ": expected " + expected + " found " + t.next(num));
						break mark;
					}
					break;
				case 4:
					ops.append("t.prev").append("(").append(num).append(");").append("\n");
					ok = true;
					expected = "";
					NavigableSet<Integer> d = s.descendingSet();
					for (int i : d) {
						if (i < num) {
							if (!t.prev(num).equals("" + i)) {
								ok = false;
								expected = "" + i;
							}
							break;
						}
					}
					if (!ok) {
						System.out.println("prev for " + num + ": expected " + expected + " found " + t.prev(num));
						break mark;
					}
					break;
			}
		}
//		System.out.println(ops);
		System.out.println(t.inOrder());
		System.out.println(1);
	}
	
	public static void solve() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		AVLTree t = new AVLTree();
		String line;
		while ((line = in.readLine()) != null) {
			String[] par = line.trim().split(" +");
			String cmd = par[0];
			int val = Integer.parseInt(par[1]);
			if (cmd.equals("add")) {
				t.insert(val);
			} else if (cmd.equals("delete")) {
				t.delete(val);
			} else if (cmd.equals("exists")) {
				System.out.println(t.exists(val));
			} else if (cmd.equals("next")) {
				System.out.println(t.next(val));
			} else if (cmd.equals("prev")) {
				System.out.println(t.prev(val));
			}
		}
	}
}
