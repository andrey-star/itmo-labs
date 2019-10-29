import java.io.*;
import java.util.Scanner;

public class E_BracketSequence {

	private static class StackLinkedList {
		private int size;
		private Node tail;

		private StackLinkedList() {
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

		private boolean isEmpty() {
			return size == 0;
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
	}


	public static void main(String[] args) throws IOException {
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("brackets.in")));
		PrintWriter out = new PrintWriter(new File("brackets.out"));
		String seq;
		while ((seq = reader.readLine()) != null) {
			StackLinkedList sl = new StackLinkedList();
			boolean inv = false;
			for (int i = 0; i < seq.length(); i++) {
				char c = seq.charAt(i);
				if (c == '(' || c == '[') {
					sl.push(c);
				} else if (c == ')') {
					if (sl.size == 0) {
						inv = true;
						break;
					}
					if (sl.peek() == '(') {
						sl.pop();
					} else {
						sl.push(c);
					}
				} else if (c == ']') {
					if (sl.size == 0) {
						inv = true;
						break;
					}
					if (sl.peek() == '[') {
						sl.pop();
					} else {
						sl.push(c);
					}
				}
			}
			out.println(sl.isEmpty() && !inv ? "YES" : "NO");
		}
		reader.close();
		out.close();
	}
}
