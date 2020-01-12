import java.io.*;
import java.util.*;

public class E_Treap {
	
	class Node {
		int key;
		int priority;
		int startIndex;
		Node left, right, parent;
		
		Node(int key, int priority, int index) {
			this.key = key;
			this.priority = priority;
			this.startIndex = index;
		}
	}
	
	class Triple {
		int parent;
		int left;
		int right;
		
		Triple(int parent, int left, int right) {
			this.parent = parent;
			this.left = left;
			this.right = right;
		}
		
	}
	
	public static void main(String[] args) throws IOException {
		new E_Treap().n();
	}
	
	private void n() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		Node[] nodes = new Node[n];
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			int k = Integer.parseInt(line[0]);
			int p = Integer.parseInt(line[1]);
			nodes[i] = new Node(k, p, i);
		}
		Arrays.sort(nodes, Comparator.comparing(o -> o.key));
		Node lastAdded = nodes[0];
		
		Triple[] res = new Triple[n];
		for (int i = 0; i < res.length; i++) {
			res[i] = new Triple(-1, -1, -1);
		}
		for (int i = 1; i < n; i++) {
			Node add = nodes[i];
			Node prevRight = null;
			Node par = lastAdded;
			while (par != null && par.priority > add.priority) {
				prevRight = par;
				par = par.parent;
			}
			if (par == null) {
				res[add.startIndex].parent = -1;
			} else {
				par.right = add;
				add.parent = par;
				res[par.startIndex].right = add.startIndex;
				res[add.startIndex].parent = par.startIndex;
			}
			if (prevRight != null) {
				prevRight.parent = add;
				add.left = prevRight;
				res[prevRight.startIndex].parent = add.startIndex;
				res[add.startIndex].left = prevRight.startIndex;
			}
			lastAdded = add;
		}
		PrintWriter out = new PrintWriter(System.out);
		out.println("YES");
		for (Triple re : res) {
			out.println(re.parent + 1 + " " + (re.left + 1) + " " + (re.right + 1));
		}
		out.close();
	}
}
