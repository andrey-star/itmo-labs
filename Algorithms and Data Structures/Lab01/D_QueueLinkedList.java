import java.io.*;

public class D_QueueLinkedList {
	
	private int size;
	private Node tail;
	private Node head;
	
	private D_QueueLinkedList() {
		size = 0;
		tail = null;
		head = null;
	}
	
	private void push(int i) {
		Node el = tail;
		tail = new Node(null, i);
		if (size == 0) {
			head = tail;
		} else {
			el.prev = tail;
		}
		size++;
	}
	
	private void pop() {
		if (size == 0) {
			throw new IndexOutOfBoundsException();
		}
		size--;
		head = head.prev;
		if (size == 0) {
			tail = head;
		}
	}
	
	private int peek() {
		return head.item;
	}
	
	@Override
	public String toString() {
		return "QueueLinkedList{" +
				"size=" + size +
				", tail=" + tail +
				", head=" + head +
				'}';
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
			return item + "";
		}
	}
	
	public static void main(String[] args) throws IOException {
		D_QueueLinkedList ql = new D_QueueLinkedList();
		BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("queue2.in")));
		PrintWriter out = new PrintWriter(new File("queue2.out"));
		int n = Integer.parseInt(reader.readLine());
		for (int i = 0; i < n; i++) {
			String[] cmd = reader.readLine().split(" ");
			if (cmd[0].equals("+")) {
				ql.push(Integer.parseInt(cmd[1]));
			} else {
				out.println(ql.peek());
				ql.pop();
			}
		}
		reader.close();
		out.close();
	}
}
