import java.io.*;

public class B_StackLinkedList {

	private int size;
	private Node tail;

	private B_StackLinkedList() {
		size = 0;
		tail = null;
	}

	private void push(int i) {
		if (size == 0) {
			tail = new Node(null, i);
		} else {
			tail = new Node(tail, i);
		}
		size++;
	}

	private void pop() {
		if (size == 0) {
			throw new IndexOutOfBoundsException();
		}
		size--;
		tail = tail.prev;
	}

	private int peek() {
		return tail.item;
	}

	@Override
	public String toString() {
		return "StackLinkedList{, tail=" + tail + '}';
	}

	private static class Node {
		int item;
		Node prev;

		Node(Node prev, int element) {
			this.item = element;
			this.prev = prev;
		}

		@Override
		public String toString() {
			return "Node{item=" + item + ", prev=" + prev + '}';
		}
	}

	public static void main(String[] args) throws IOException {
		B_StackLinkedList sl = new B_StackLinkedList();
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("stack2.in")));
		PrintWriter out = new PrintWriter(new File("stack2.out"));
		int n = Integer.parseInt(reader.readLine());
		for (int i = 0; i < n; i++) {
			String[] cmd = reader.readLine().split(" ");
			if (cmd[0].equals("+")) {
				sl.push(Integer.parseInt(cmd[1]));
			} else {
				out.println(sl.peek());
				sl.pop();
			}
		}
		reader.close();
		out.close();
	}
}
