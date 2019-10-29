package queue;

import java.util.ArrayList;

public class LinkedQueue extends AbstractQueue {
	
	class Node {
		Node next;
		Object value;
		
		Node(Node next, Object value) {
			this.next = next;
			this.value = value;
		}
	}
	
	private Node head;
	private Node tail;
	
	@Override
	public void enqueueImpl(Object o) {
		if (size == 0) {
			tail = new Node(null, o);
			head = tail;
		} else {
			tail.next = new Node(null, o);
			tail = tail.next;
		}
	}
	
	@Override
	public void dequeueImpl() {
		head = head.next;
		if (size == 1) {
			tail = null;
		}
	}
	
	@Override
	public Object elementImpl() {
		return head.value;
	}
	
	@Override
	protected LinkedQueue makeCopy() {
		LinkedQueue queue = new LinkedQueue();
		Node temp = head;
		while (temp != null) {
			queue.enqueue(temp.value);
			temp = temp.next;
		}
		return queue;
	}
	
	@Override
	public void clearImpl() {
		ArrayList
		head = null;
		tail = null;
	}
	
}
